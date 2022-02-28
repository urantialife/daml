// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.participant.state.kvutils.api

import akka.NotUsed
import akka.stream.scaladsl.Source
import com.daml.ledger.api.health.HealthStatus
import com.daml.ledger.configuration.LedgerInitialConditions
import com.daml.ledger.offset.Offset
import com.daml.ledger.participant.state.kvutils.store.{DamlLogEntry, DamlLogEntryId}
import com.daml.ledger.participant.state.kvutils.{Envelope, KVOffset, KeyValueConsumption}
import com.daml.ledger.participant.state.v2._
import com.daml.ledger.validator.preexecution.TimeUpdatesProvider
import com.daml.lf.data.Time
import com.daml.lf.data.Time.Timestamp
import com.daml.logging.LoggingContext
import com.daml.metrics.{Metrics, Timed}

/** Adapts a [[LedgerReader]] instance to [[ReadService]].
  * Performs translation between the offsets required by the underlying reader and [[ReadService]]:
  *
  *   - a 3 component integer offset is exposed to [[ReadService]] (see [[com.daml.ledger.participant.state.kvutils.KVOffsetBuilder]]),
  *   - a max. 2 component integer offset is expected from the underlying [[LedgerReader]], and
  *   - the third (lowest index) component is generated as the index of the update in case more than 1 has been generated by [[KeyValueConsumption.logEntryToUpdate]],
  *   - otherwise the offset is passed on to [[ReadService]] as-is.
  */
class KeyValueParticipantStateReader private[api] (
    reader: LedgerReader,
    metrics: Metrics,
    logEntryToUpdate: (
        DamlLogEntryId,
        DamlLogEntry,
        Option[Timestamp],
    ) => LoggingContext => List[Update],
    timeUpdatesProvider: TimeUpdatesProvider,
    failOnUnexpectedEvent: Boolean,
) extends ReadService {

  import KeyValueParticipantStateReader._

  override def ledgerInitialConditions(): Source[LedgerInitialConditions, NotUsed] =
    Source.single(createLedgerInitialConditions())

  override def stateUpdates(
      beginAfter: Option[Offset]
  )(implicit loggingContext: LoggingContext): Source[(Offset, Update), NotUsed] = {
    Source
      .single(beginAfter.map(offset => KVOffset(offset).zeroLowest.offset))
      .flatMapConcat(reader.events)
      .flatMapConcat { case LedgerRecord(offset, entryId, envelope) =>
        Timed
          .value(metrics.daml.kvutils.reader.openEnvelope, Envelope.open(envelope))
          .flatMap {
            case Envelope.LogEntryMessage(logEntry) =>
              Timed.value(
                metrics.daml.kvutils.reader.parseUpdates, {
                  val logEntryId = DamlLogEntryId.parseFrom(entryId.bytes)
                  val updates =
                    logEntryToUpdate(
                      logEntryId,
                      logEntry,
                      timeUpdatesProvider(),
                    )(loggingContext)
                  val updatesWithOffsets =
                    Source(updates).zipWithIndex.map { case (update, index) =>
                      offsetForUpdate(offset, index.toInt, updates.size) -> update
                    }
                  Right(updatesWithOffsets)
                },
              )
            case _ =>
              if (failOnUnexpectedEvent)
                Left("Envelope does not contain a log entry")
              else
                Right(Source.empty)
          }
          .getOrElse(
            throw new IllegalArgumentException(s"Invalid log entry received at offset $offset")
          )
      }
  }

  override def currentHealth(): HealthStatus =
    reader.currentHealth()

  private def createLedgerInitialConditions(): LedgerInitialConditions =
    LedgerInitialConditions(
      reader.ledgerId(),
      LedgerReader.DefaultConfiguration,
      Time.Timestamp.Epoch,
    )
}

object KeyValueParticipantStateReader {
  def apply(
      reader: LedgerReader,
      metrics: Metrics,
      timeUpdatesProvider: TimeUpdatesProvider = TimeUpdatesProvider.ReasonableDefault,
      failOnUnexpectedEvent: Boolean = true,
  ): KeyValueParticipantStateReader =
    new KeyValueParticipantStateReader(
      reader,
      metrics,
      KeyValueConsumption.logEntryToUpdate,
      timeUpdatesProvider,
      failOnUnexpectedEvent,
    )

  private[api] def offsetForUpdate(
      offsetFromRecord: Offset,
      index: Int,
      totalUpdates: Int,
  ): Offset =
    if (totalUpdates > 1) {
      KVOffset(offsetFromRecord).setLowest(index).offset
    } else {
      offsetFromRecord
    }
}

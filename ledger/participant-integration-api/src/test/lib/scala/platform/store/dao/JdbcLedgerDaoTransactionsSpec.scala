// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.platform.store.dao

import akka.NotUsed
import akka.stream.scaladsl.{Sink, Source}
import com.daml.api.util.TimestampConversion
import com.daml.ledger.api.v1.transaction.Transaction
import com.daml.ledger.api.v1.transaction_service.GetTransactionsResponse
import com.daml.ledger.api.{v1 => lav1}
import com.daml.ledger.offset.Offset
import com.daml.ledger.resources.ResourceContext
import com.daml.lf.data.Ref.{Identifier, Party}
import com.daml.lf.ledger.EventId
import com.daml.lf.transaction.Node
import com.daml.logging.LoggingContext
import com.daml.platform.{ApiOffset, FilterRelation}
import com.daml.platform.api.v1.event.EventOps.EventOps
import com.daml.platform.participant.util.LfEngineToApi
import com.daml.platform.store.dao._
import com.daml.platform.store.entries.LedgerEntry
import org.scalacheck.Gen
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Inside, LoneElement, OptionValues}

import scala.concurrent.Future

private[dao] trait JdbcLedgerDaoTransactionsSpec extends OptionValues with Inside with LoneElement {
  this: AsyncFlatSpec with Matchers with JdbcLedgerDaoSuite =>

  import JdbcLedgerDaoTransactionsSpec._

  behavior of "JdbcLedgerDao (lookupFlatTransactionById)"

  it should "return nothing for a mismatching transaction id" in {
    for {
      (_, tx) <- store(singleCreate)
      result <- ledgerDao.transactionsReader
        .lookupFlatTransactionById(transactionId = "WRONG", tx.actAs.toSet)
    } yield {
      result shouldBe None
    }
  }

  it should "return nothing for a mismatching party" in {
    for {
      (_, tx) <- store(singleCreate)
      result <- ledgerDao.transactionsReader
        .lookupFlatTransactionById(tx.transactionId, Set("WRONG"))
    } yield {
      result shouldBe None
    }
  }

  it should "return the expected flat transaction for a correct request (create)" in {
    for {
      (offset, tx) <- store(singleCreate)
      result <- ledgerDao.transactionsReader
        .lookupFlatTransactionById(tx.transactionId, tx.actAs.toSet)
    } yield {
      inside(result.value.transaction) { case Some(transaction) =>
        transaction.commandId shouldBe tx.commandId.get
        transaction.offset shouldBe ApiOffset.toApiString(offset)
        TimestampConversion.toLf(
          transaction.effectiveAt.value,
          TimestampConversion.ConversionMode.Exact,
        ) shouldBe tx.ledgerEffectiveTime
        transaction.transactionId shouldBe tx.transactionId
        transaction.workflowId shouldBe tx.workflowId.getOrElse("")
        inside(transaction.events.loneElement.event.created) { case Some(created) =>
          val (nodeId, createNode: Node.Create) =
            tx.transaction.nodes.head
          created.eventId shouldBe EventId(tx.transactionId, nodeId).toLedgerString
          created.witnessParties should contain only (tx.actAs: _*)
          created.agreementText.getOrElse("") shouldBe createNode.agreementText
          created.contractKey shouldBe None
          created.createArguments shouldNot be(None)
          created.signatories should contain theSameElementsAs createNode.signatories
          created.observers should contain theSameElementsAs createNode.stakeholders.diff(
            createNode.signatories
          )
          created.templateId shouldNot be(None)
        }
      }
    }
  }

  it should "return the expected flat transaction for a correct request (exercise)" in {
    for {
      (_, create) <- store(singleCreate)
      (offset, exercise) <- store(singleExercise(nonTransient(create).loneElement))
      result <- ledgerDao.transactionsReader
        .lookupFlatTransactionById(exercise.transactionId, exercise.actAs.toSet)
    } yield {
      inside(result.value.transaction) { case Some(transaction) =>
        transaction.commandId shouldBe exercise.commandId.get
        transaction.offset shouldBe ApiOffset.toApiString(offset)
        transaction.transactionId shouldBe exercise.transactionId
        TimestampConversion.toLf(
          transaction.effectiveAt.value,
          TimestampConversion.ConversionMode.Exact,
        ) shouldBe exercise.ledgerEffectiveTime
        transaction.workflowId shouldBe exercise.workflowId.getOrElse("")
        inside(transaction.events.loneElement.event.archived) { case Some(archived) =>
          val (nodeId, exerciseNode: Node.Exercise) =
            exercise.transaction.nodes.head
          archived.eventId shouldBe EventId(transaction.transactionId, nodeId).toLedgerString
          archived.witnessParties should contain only (exercise.actAs: _*)
          archived.contractId shouldBe exerciseNode.targetCoid.coid
          archived.templateId shouldNot be(None)
        }
      }
    }
  }

  it should "show command IDs to the original submitters" in {
    val signatories = Set(alice, bob)
    val stakeholders = Set(alice, bob, charlie) // Charlie is only stakeholder
    val actAs = List(alice, bob, david) // David is submitter but not signatory
    for {
      (_, tx) <- store(singleCreate(createNode(_, signatories, stakeholders), actAs))
      // Response 1: querying as all submitters
      result1 <- ledgerDao.transactionsReader
        .lookupFlatTransactionById(tx.transactionId, Set(alice, bob, david))
      // Response 2: querying as a proper subset of all submitters
      result2 <- ledgerDao.transactionsReader
        .lookupFlatTransactionById(tx.transactionId, Set(alice, david))
      // Response 3: querying as a proper superset of all submitters
      result3 <- ledgerDao.transactionsReader
        .lookupFlatTransactionById(tx.transactionId, Set(alice, bob, charlie, david))
    } yield {
      result1.value.transaction.value.commandId shouldBe tx.commandId.get
      result2.value.transaction.value.commandId shouldBe tx.commandId.get
      result3.value.transaction.value.commandId shouldBe tx.commandId.get
    }
  }

  it should "hide command IDs from non-submitters" in {
    val signatories = Set(alice, bob)
    val stakeholders = Set(alice, bob, charlie) // Charlie is only stakeholder
    val actAs = List(alice, bob, david) // David is submitter but not signatory
    for {
      (_, tx) <- store(singleCreate(createNode(_, signatories, stakeholders), actAs))
      result <- ledgerDao.transactionsReader
        .lookupFlatTransactionById(tx.transactionId, Set(charlie))
    } yield {
      result.value.transaction.value.commandId shouldBe ""
    }
  }

  it should "hide events on transient contracts to the original submitter" in {
    for {
      (offset, tx) <- store(fullyTransient())
      result <- ledgerDao.transactionsReader
        .lookupFlatTransactionById(tx.transactionId, tx.actAs.toSet)
    } yield {
      inside(result.value.transaction) { case Some(transaction) =>
        transaction.commandId shouldBe tx.commandId.get
        transaction.offset shouldBe ApiOffset.toApiString(offset)
        transaction.transactionId shouldBe tx.transactionId
        TimestampConversion.toLf(
          transaction.effectiveAt.value,
          TimestampConversion.ConversionMode.Exact,
        ) shouldBe tx.ledgerEffectiveTime
        transaction.workflowId shouldBe tx.workflowId.getOrElse("")
        transaction.events shouldBe Seq.empty
      }
    }
  }

  behavior of "JdbcLedgerDao (getFlatTransactions)"

  it should "match the results of lookupFlatTransactionById" in {
    for {
      (from, to, transactions) <- storeTestFixture()
      lookups <- lookupIndividually(transactions, Set(alice, bob, charlie))
      result <- transactionsOf(
        ledgerDao.transactionsReader
          .getFlatTransactions(
            startExclusive = from,
            endInclusive = to,
            filter = Map(alice -> Set.empty, bob -> Set.empty, charlie -> Set.empty),
            verbose = true,
          )
      )
    } yield {
      comparable(result) should contain theSameElementsInOrderAs comparable(lookups)
    }
  }

  it should "filter correctly by party" in {
    for {
      from <- ledgerDao.lookupLedgerEnd()
      (_, tx) <- store(
        multipleCreates(
          charlie,
          Seq(
            (alice, someTemplateId, someContractArgument),
            (bob, someTemplateId, someContractArgument),
          ),
        )
      )
      to <- ledgerDao.lookupLedgerEnd()
      individualLookupForAlice <- lookupIndividually(Seq(tx), as = Set(alice))
      individualLookupForBob <- lookupIndividually(Seq(tx), as = Set(bob))
      individualLookupForCharlie <- lookupIndividually(Seq(tx), as = Set(charlie))
      resultForAlice <- transactionsOf(
        ledgerDao.transactionsReader
          .getFlatTransactions(
            startExclusive = from.lastOffset,
            endInclusive = to.lastOffset,
            filter = Map(alice -> Set.empty),
            verbose = true,
          )
      )
      resultForBob <- transactionsOf(
        ledgerDao.transactionsReader
          .getFlatTransactions(
            startExclusive = from.lastOffset,
            endInclusive = to.lastOffset,
            filter = Map(bob -> Set.empty),
            verbose = true,
          )
      )
      resultForCharlie <- transactionsOf(
        ledgerDao.transactionsReader
          .getFlatTransactions(
            startExclusive = from.lastOffset,
            endInclusive = to.lastOffset,
            filter = Map(charlie -> Set.empty),
            verbose = true,
          )
      )
    } yield {
      individualLookupForAlice should contain theSameElementsInOrderAs resultForAlice
      individualLookupForBob should contain theSameElementsInOrderAs resultForBob
      individualLookupForCharlie should contain theSameElementsInOrderAs resultForCharlie
    }
  }

  it should "filter correctly for a single party" in {
    for {
      from <- ledgerDao.lookupLedgerEnd()
      _ <- store(
        multipleCreates(
          operator = "operator",
          signatoriesAndTemplates = Seq(
            (alice, someTemplateId, someContractArgument),
            (bob, otherTemplateId, otherContractArgument),
            (alice, otherTemplateId, otherContractArgument),
          ),
        )
      )
      to <- ledgerDao.lookupLedgerEnd()
      result <- transactionsOf(
        ledgerDao.transactionsReader
          .getFlatTransactions(
            startExclusive = from.lastOffset,
            endInclusive = to.lastOffset,
            filter = Map(alice -> Set(otherTemplateId)),
            verbose = true,
          )
      )
    } yield {
      inside(result.loneElement.events.loneElement.event.created) { case Some(create) =>
        create.witnessParties.loneElement shouldBe alice
        create.templateId.value shouldBe LfEngineToApi.toApiIdentifier(otherTemplateId)
      }
    }
  }

  it should "filter correctly by multiple parties with the same template" in {
    for {
      from <- ledgerDao.lookupLedgerEnd()
      _ <- store(
        multipleCreates(
          operator = "operator",
          signatoriesAndTemplates = Seq(
            (alice, someTemplateId, someContractArgument),
            (bob, otherTemplateId, otherContractArgument),
            (alice, otherTemplateId, otherContractArgument),
          ),
        )
      )
      to <- ledgerDao.lookupLedgerEnd()
      result <- transactionsOf(
        ledgerDao.transactionsReader
          .getFlatTransactions(
            startExclusive = from.lastOffset,
            endInclusive = to.lastOffset,
            filter = Map(
              alice -> Set(otherTemplateId),
              bob -> Set(otherTemplateId),
            ),
            verbose = true,
          )
      )
    } yield {
      val events = result.loneElement.events.toArray
      events should have length 2
      inside(events(0).event.created) { case Some(create) =>
        create.witnessParties.loneElement shouldBe bob
        create.templateId.value shouldBe LfEngineToApi.toApiIdentifier(otherTemplateId)
      }
      inside(events(1).event.created) { case Some(create) =>
        create.witnessParties.loneElement shouldBe alice
        create.templateId.value shouldBe LfEngineToApi.toApiIdentifier(otherTemplateId)
      }
    }
  }

  it should "filter correctly by multiple parties with different templates" in {
    for {
      from <- ledgerDao.lookupLedgerEnd()
      _ <- store(
        multipleCreates(
          operator = "operator",
          signatoriesAndTemplates = Seq(
            (alice, someTemplateId, someContractArgument),
            (bob, otherTemplateId, otherContractArgument),
            (alice, otherTemplateId, otherContractArgument),
          ),
        )
      )
      to <- ledgerDao.lookupLedgerEnd()
      result <- transactionsOf(
        ledgerDao.transactionsReader
          .getFlatTransactions(
            startExclusive = from.lastOffset,
            endInclusive = to.lastOffset,
            filter = Map(
              alice -> Set(someTemplateId),
              bob -> Set(otherTemplateId),
            ),
            verbose = true,
          )
      )
    } yield {
      val events = result.loneElement.events.toArray
      events should have length 2
      inside(events(0).event.created) { case Some(create) =>
        create.witnessParties.loneElement shouldBe alice
        create.templateId.value shouldBe LfEngineToApi.toApiIdentifier(someTemplateId)
      }
      inside(events(1).event.created) { case Some(create) =>
        create.witnessParties.loneElement shouldBe bob
        create.templateId.value shouldBe LfEngineToApi.toApiIdentifier(otherTemplateId)
      }
    }
  }

  it should "filter correctly by multiple parties with different template and wildcards" in {
    for {
      from <- ledgerDao.lookupLedgerEnd()
      (_, _) <- store(
        multipleCreates(
          operator = "operator",
          signatoriesAndTemplates = Seq(
            (alice, someTemplateId, someContractArgument),
            (bob, otherTemplateId, otherContractArgument),
            (alice, otherTemplateId, otherContractArgument),
          ),
        )
      )
      to <- ledgerDao.lookupLedgerEnd()
      result <- transactionsOf(
        ledgerDao.transactionsReader
          .getFlatTransactions(
            startExclusive = from.lastOffset,
            endInclusive = to.lastOffset,
            filter = Map(
              alice -> Set(otherTemplateId),
              bob -> Set.empty,
            ),
            verbose = true,
          )
      )
    } yield {
      val events = result.loneElement.events.toArray
      events should have length 2
      inside(events(0).event.created) { case Some(create) =>
        create.witnessParties.loneElement shouldBe bob
        create.templateId.value shouldBe LfEngineToApi.toApiIdentifier(otherTemplateId)
      }
      inside(events(1).event.created) { case Some(create) =>
        create.witnessParties.loneElement shouldBe alice
        create.templateId.value shouldBe LfEngineToApi.toApiIdentifier(otherTemplateId)
      }
    }
  }

  it should "return all events in the expected order" in {
    for {
      from <- ledgerDao.lookupLedgerEnd()
      (_, create) <- store(singleCreate)
      firstContractId = nonTransient(create).loneElement
      (offset, exercise) <- store(exerciseWithChild(firstContractId))
      result <- ledgerDao.transactionsReader
        .getFlatTransactions(
          from.lastOffset,
          offset,
          exercise.actAs.map(submitter => submitter -> Set.empty[Identifier]).toMap,
          verbose = true,
        )
        .runWith(Sink.seq)
    } yield {
      import com.daml.ledger.api.v1.event.Event
      import com.daml.ledger.api.v1.event.Event.Event.{Archived, Created}

      val txs = extractAllTransactions(result)

      inside(txs) { case Vector(tx1, tx2) =>
        tx1.transactionId shouldBe create.transactionId
        tx2.transactionId shouldBe exercise.transactionId
        inside(tx1.events) { case Seq(Event(Created(createdEvent))) =>
          createdEvent.contractId shouldBe firstContractId.coid
        }
        inside(tx2.events) { case Seq(Event(Archived(archivedEvent)), Event(Created(_))) =>
          archivedEvent.contractId shouldBe firstContractId.coid
        }
      }
    }
  }

  it should "return the expected flat transaction for the specified offset range" in {
    for {
      (_, create1) <- store(singleCreate)
      (offset1, exercise) <- store(singleExercise(nonTransient(create1).loneElement))
      (offset2, create2) <- store(singleCreate)
      result <- ledgerDao.transactionsReader
        .getFlatTransactions(
          offset1,
          offset2,
          exercise.actAs.map(submitter => submitter -> Set.empty[Identifier]).toMap,
          verbose = true,
        )
        .runWith(Sink.seq)

    } yield {
      import com.daml.ledger.api.v1.event.Event
      import com.daml.ledger.api.v1.event.Event.Event.Created

      inside(extractAllTransactions(result)) { case Vector(tx) =>
        tx.transactionId shouldBe create2.transactionId
        inside(tx.events) { case Seq(Event(Created(createdEvent))) =>
          createdEvent.contractId shouldBe nonTransient(create2).loneElement.coid
        }
      }
    }
  }

  it should "return empty source when offset range is from the future" in {
    val commands: Vector[(Offset, LedgerEntry.Transaction)] = Vector.fill(3)(singleCreate)
    val beginOffsetFromTheFuture = nextOffset()
    val endOffsetFromTheFuture = nextOffset()

    for {
      _ <- storeSync(commands)

      result <- ledgerDao.transactionsReader
        .getFlatTransactions(
          beginOffsetFromTheFuture,
          endOffsetFromTheFuture,
          Map(alice -> Set.empty[Identifier]),
          verbose = true,
        )
        .runWith(Sink.seq)

    } yield {
      extractAllTransactions(result) shouldBe empty
    }
  }

  it should "return all transactions in the specified offset range when iterating with gaps in the offsets assigned to events and a page size that ensures a page ends in such a gap" in {
    // Simulates a gap in the offsets assigned to events, as they
    // can be assigned to party allocation, package uploads and
    // configuration updates as well
    def offsetGap(): Vector[(Offset, LedgerEntry.Transaction)] = {
      nextOffset()
      Vector.empty[(Offset, LedgerEntry.Transaction)]
    }

    // the order of `nextOffset()` calls is important
    val beginOffset = nextOffset()

    val commandsWithOffsetGaps: Vector[(Offset, LedgerEntry.Transaction)] =
      Vector(singleCreate) ++ offsetGap() ++
        Vector.fill(2)(singleCreate) ++ offsetGap() ++
        Vector.fill(3)(singleCreate) ++ offsetGap() ++ offsetGap() ++
        Vector.fill(5)(singleCreate)

    val endOffset = nextOffset()

    commandsWithOffsetGaps should have length 11L

    for {
      _ <- storeSync(commandsWithOffsetGaps)

      // `pageSize = 2` and the offset gaps in the `commandWithOffsetGaps` above are to make sure
      // that streaming works with event pages separated by offsets that don't have events in the store
      response <- createLedgerDaoResourceOwner(
        pageSize = 2,
        eventsProcessingParallelism = 8,
        acsIdPageSize = 2,
        acsIdFetchingParallelism = 2,
        acsContractFetchingParallelism = 2,
        acsGlobalParallelism = 10,
      ).use(
        _.transactionsReader
          .getFlatTransactions(
            beginOffset,
            endOffset,
            Map(alice -> Set.empty[Identifier]),
            verbose = true,
          )
          .runWith(Sink.seq)
      )(ResourceContext(executionContext))

      readTxs = extractAllTransactions(response)
    } yield {
      val readTxOffsets: Vector[String] = readTxs.map(_.offset)
      readTxOffsets shouldBe readTxOffsets.sorted
      readTxOffsets shouldBe commandsWithOffsetGaps.map(_._1.toHexString)
    }
  }

  // TODO need to find out why this is so slow to execute on Oracle
  it should "fall back to limit-based query with consistent results" in {
    val txSeqLength = 1000
    txSeqTrial(
      trials = 10,
      txSeq = unfilteredTxSeq(length = txSeqLength),
      codePath = Gen oneOf getFlatTransactionCodePaths,
    )
  }

  private[this] def txSeqTrial(
      trials: Int,
      txSeq: Gen[Vector[Boolean]],
      codePath: Gen[FlatTransactionCodePath],
  ) = {
    import JdbcLedgerDaoSuite._
    import scalaz.std.list._
    import scalaz.std.scalaFuture._

    val trialData = Gen
      .listOfN(trials, Gen.zip(txSeq, codePath))
      .sample getOrElse sys.error("impossible Gen failure")

    trialData
      .traverseFM { case (boolSeq, cp) =>
        for {
          from <- ledgerDao.lookupLedgerEnd()
          commands <- storeSync(boolSeq map (if (_) cp.makeMatching() else cp.makeNonMatching()))
          matchingOffsets = commands zip boolSeq collect { case ((off, _), true) =>
            off.toHexString
          }
          to <- ledgerDao.lookupLedgerEnd()
          response <- ledgerDao.transactionsReader
            .getFlatTransactions(from.lastOffset, to.lastOffset, cp.filter, verbose = false)
            .runWith(Sink.seq)
          readOffsets = response flatMap { case (_, gtr) => gtr.transactions map (_.offset) }
          readCreates = extractAllTransactions(response) flatMap (_.events)
        } yield try {
          readCreates.size should ===(boolSeq count identity)
          // we check that the offsets from the DB match the ones we had before
          // submission as a substitute for actually inspecting the events (indeed,
          // so many of the events are = as written that this would not be useful)
          readOffsets should ===(matchingOffsets)
        } catch {
          case ae: org.scalatest.exceptions.TestFailedException =>
            throw ae modifyMessage (_ map { msg: String =>
              msg +
                "\n  Random parameters:" +
                s"\n    actual frequency: ${boolSeq.count(identity)}/${boolSeq.size}" +
                s"\n    code path: ${cp.label}" +
                s"\n  Please copy the above 4 lines to https://github.com/digital-asset/daml/issues/7521" +
                s"\n  along with which of (JdbcLedgerDaoPostgresqlSpec, JdbcLedgerDaoH2DatabaseSpec) failed"
            })
        }
      }
      .map(_.foldLeft(succeed)((_, r) => r))
  }

  /*
  it should "get all transactions in order, 48%, onlyWildcardParties" in {
    val frequency = 48
    val txSeqLength = 1000
    val path = "onlyWildcardParties"
    txSeqTrial(
      250,
      unfilteredTxFrequencySeq(txSeqLength, frequencyPct = frequency),
      getFlatTransactionCodePaths find (_.label == path) getOrElse fail(s"$path not found"))
  }
   */

  private def storeTestFixture(): Future[(Offset, Offset, Seq[LedgerEntry.Transaction])] =
    for {
      from <- ledgerDao.lookupLedgerEnd()
      (_, t1) <- store(singleCreate)
      (_, t2) <- store(singleCreate)
      (_, t3) <- store(singleExercise(nonTransient(t2).loneElement))
      (_, t4) <- store(fullyTransient())
      to <- ledgerDao.lookupLedgerEnd()
    } yield (from.lastOffset, to.lastOffset, Seq(t1, t2, t3, t4))

  private def lookupIndividually(
      transactions: Seq[LedgerEntry.Transaction],
      as: Set[Party],
  ): Future[Seq[Transaction]] =
    Future
      .sequence(
        transactions.map(tx =>
          ledgerDao.transactionsReader
            .lookupFlatTransactionById(tx.transactionId, as)
        )
      )
      .map(_.flatMap(_.toList.flatMap(_.transaction.toList)))

  private def transactionsOf(
      source: Source[(Offset, GetTransactionsResponse), NotUsed]
  ): Future[Seq[Transaction]] =
    source
      .map(_._2)
      .runWith(Sink.seq)
      .map(_.flatMap(_.transactions))

  // Ensure two sequences of transactions are comparable:
  // - witnesses do not have to appear in a specific order
  private def comparable(txs: Seq[Transaction]): Seq[Transaction] =
    txs.map(tx => tx.copy(events = tx.events.map(_.modifyWitnessParties(_.sorted))))

  private def extractAllTransactions(
      responses: Seq[(Offset, GetTransactionsResponse)]
  ): Vector[Transaction] =
    responses.foldLeft(Vector.empty[Transaction])((b, a) => b ++ a._2.transactions.toVector)

  private def createLedgerDaoResourceOwner(
      pageSize: Int,
      eventsProcessingParallelism: Int,
      acsIdPageSize: Int,
      acsIdFetchingParallelism: Int,
      acsContractFetchingParallelism: Int,
      acsGlobalParallelism: Int,
  ) =
    LoggingContext.newLoggingContext { implicit loggingContext =>
      daoOwner(
        eventsPageSize = pageSize,
        eventsProcessingParallelism = eventsProcessingParallelism,
        acsIdPageSize = acsIdPageSize,
        acsIdFetchingParallelism = acsIdFetchingParallelism,
        acsContractFetchingParallelism = acsContractFetchingParallelism,
        acsGlobalParallelism = acsGlobalParallelism,
      )
    }

  // XXX SC much of this is repeated because we're more concerned here
  // with whether each query is tested than whether the specifics of the
  // predicate are accurate. To test the latter, the creation would have
  // to have much more detail and should be a pair of Gen[Offset => LedgerEntry.Transaction]
  // rather than a pair of simple side-effecting procedures that always
  // produce more or less the same data.  If we aren't interested in testing
  // the latter at any point, we can remove most of this.
  private val getFlatTransactionCodePaths: Seq[FlatTransactionCodePath] = {
    import JdbcLedgerDaoTransactionsSpec.{FlatTransactionCodePath => Mk}
    Seq(
      Mk(
        "singleWildcardParty",
        Map(alice -> Set.empty),
        () => singleCreate(create(_, signatories = Set(alice))),
        () => singleCreate(create(_, signatories = Set(bob))),
        ce => (ce.signatories ++ ce.observers) contains alice,
      ),
      Mk(
        "singlePartyWithTemplates",
        Map(alice -> Set(someTemplateId)),
        () => singleCreate(create(_, signatories = Set(alice))),
        () => singleCreate(create(_, signatories = Set(bob))),
        ce =>
          ((ce.signatories ++ ce.observers) contains alice), /*TODO && ce.templateId == Some(someTemplateId.toString)*/
      ),
      Mk(
        "onlyWildcardParties",
        Map(alice -> Set.empty, bob -> Set.empty),
        () => singleCreate(create(_, signatories = Set(alice))),
        () => singleCreate(create(_, signatories = Set(charlie))),
        ce => (ce.signatories ++ ce.observers) exists Set(alice, bob),
      ),
      Mk(
        "sameTemplates",
        Map(alice -> Set(someTemplateId), bob -> Set(someTemplateId)),
        () => singleCreate(create(_, signatories = Set(alice))),
        () => singleCreate(create(_, signatories = Set(charlie))),
        ce => (ce.signatories ++ ce.observers) exists Set(alice, bob),
      ),
      Mk(
        "mixedTemplates",
        Map(alice -> Set(someTemplateId), bob -> Set(otherTemplateId)),
        () => singleCreate(create(_, signatories = Set(alice))),
        () => singleCreate(create(_, signatories = Set(charlie))),
        ce => (ce.signatories ++ ce.observers) exists Set(alice, bob),
      ),
      Mk(
        "mixedTemplatesWithWildcardParties",
        Map(alice -> Set(someTemplateId), bob -> Set.empty),
        () => singleCreate(create(_, signatories = Set(alice))),
        () => singleCreate(create(_, signatories = Set(charlie))),
        ce => (ce.signatories ++ ce.observers) exists Set(alice, bob),
      ),
    )
  }
}

private[dao] object JdbcLedgerDaoTransactionsSpec {
  private final case class FlatTransactionCodePath(
      label: String,
      filter: FilterRelation,
      makeMatching: () => (Offset, LedgerEntry.Transaction),
      makeNonMatching: () => (Offset, LedgerEntry.Transaction),
      // XXX SC we don't need discriminate unless we test the event contents
      // instead of just the offsets
      discriminate: lav1.event.CreatedEvent => Boolean = _ => false,
  )

  private def unfilteredTxSeq(length: Int): Gen[Vector[Boolean]] =
    Gen.oneOf(1, 2, 5, 10, 20, 50, 100) flatMap { invFreq =>
      unfilteredTxFrequencySeq(length, frequencyPct = 100 / invFreq)
    }

  private def unfilteredTxFrequencySeq(length: Int, frequencyPct: Int): Gen[Vector[Boolean]] =
    Gen.containerOfN[Vector, Boolean](
      length,
      Gen.frequency((frequencyPct, true), (100 - frequencyPct, false)),
    )
}

// Copyright (c) 2022 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.http

import com.daml.http.domain.TemplateId.RequiredPkg
import com.daml.lf.data.ImmArray.ImmArraySeq
import com.daml.http.domain.{
  ActiveContract,
  Choice,
  Contract,
  CreateAndExerciseCommand,
  CreateCommand,
  ExerciseCommand,
  ExerciseResponse,
  JwtWritePayload,
  TemplateId,
}
import com.daml.http.util.ClientUtil.uniqueCommandId
import com.daml.http.util.FutureUtil._
import com.daml.http.util.IdentifierConverters.refApiIdentifier
import com.daml.http.util.Logging.{InstanceUUID, RequestID}
import com.daml.http.util.{Commands, Transactions}
import LedgerClientJwt.Grpc
import com.daml.jwt.domain.Jwt
import com.daml.ledger.api.refinements.{ApiTypes => lar}
import com.daml.ledger.api.{v1 => lav1}
import com.daml.logging.LoggingContextOf.{label, withEnrichedLoggingContext}
import com.daml.logging.{ContextualizedLogger, LoggingContextOf}
import scalaz.std.scalaFuture._
import scalaz.syntax.show._
import scalaz.syntax.std.option._
import scalaz.syntax.traverse._
import scalaz.{-\/, EitherT, \/, \/-}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class CommandService(
    submitAndWaitForTransaction: LedgerClientJwt.SubmitAndWaitForTransaction,
    submitAndWaitForTransactionTree: LedgerClientJwt.SubmitAndWaitForTransactionTree,
)(implicit ec: ExecutionContext) {

  import CommandService._

  private def withTemplateLoggingContext[T](
      templateId: TemplateId.RequiredPkg
  )(implicit lc: LoggingContextOf[T]): withEnrichedLoggingContext[TemplateId.RequiredPkg, T] =
    withEnrichedLoggingContext(
      label[TemplateId.RequiredPkg],
      "template_id" -> templateId.toString,
    )

  private def withTemplateChoiceLoggingContext[T](
      templateId: TemplateId.RequiredPkg,
      choice: domain.Choice,
  )(implicit lc: LoggingContextOf[T]): withEnrichedLoggingContext[Choice, RequiredPkg with T] =
    withTemplateLoggingContext(templateId).run(
      withEnrichedLoggingContext(
        label[domain.Choice],
        "choice" -> choice.toString,
      )(_)
    )

  def create(
      jwt: Jwt,
      jwtPayload: JwtWritePayload,
      input: CreateCommand[lav1.value.Record, TemplateId.RequiredPkg],
  )(implicit
      lc: LoggingContextOf[InstanceUUID with RequestID]
  ): Future[Error \/ ActiveContract[lav1.value.Value]] =
    withTemplateLoggingContext(input.templateId).run { implicit lc =>
      logger.trace(s"sending create command to ledger")
      val command = createCommand(input)
      val request = submitAndWaitRequest(jwtPayload, input.meta, command, "create")
      val et: ET[ActiveContract[lav1.value.Value]] = for {
        response <- logResult(Symbol("create"), submitAndWaitForTransaction(jwt, request))
        contract <- either(exactlyOneActiveContract(response))
      } yield contract
      et.run
    }

  def exercise(
      jwt: Jwt,
      jwtPayload: JwtWritePayload,
      input: ExerciseCommand[lav1.value.Value, ExerciseCommandRef],
  )(implicit
      lc: LoggingContextOf[InstanceUUID with RequestID]
  ): Future[Error \/ ExerciseResponse[lav1.value.Value]] =
    withEnrichedLoggingContext(
      label[lav1.value.Value],
      "contract_id" -> input.argument.getContractId,
    ).run(implicit lc =>
      withTemplateChoiceLoggingContext(input.reference.fold(_._1, _._1), input.choice)
        .run { implicit lc =>
          logger.trace("sending exercise command to ledger")
          val command = exerciseCommand(input)
          val request = submitAndWaitRequest(jwtPayload, input.meta, command, "exercise")

          val et: ET[ExerciseResponse[lav1.value.Value]] = for {
            response <-
              logResult(Symbol("exercise"), submitAndWaitForTransactionTree(jwt, request))
            exerciseResult <- either(exerciseResult(response))
            contracts <- either(contracts(response))
          } yield ExerciseResponse(exerciseResult, contracts)

          et.run
        }
    )

  def createAndExercise(
      jwt: Jwt,
      jwtPayload: JwtWritePayload,
      input: CreateAndExerciseCommand[lav1.value.Record, lav1.value.Value, TemplateId.RequiredPkg],
  )(implicit
      lc: LoggingContextOf[InstanceUUID with RequestID]
  ): Future[Error \/ ExerciseResponse[lav1.value.Value]] =
    withTemplateChoiceLoggingContext(input.templateId, input.choice).run { implicit lc =>
      logger.trace("sending create and exercise command to ledger")
      val command = createAndExerciseCommand(input)
      val request = submitAndWaitRequest(jwtPayload, input.meta, command, "createAndExercise")
      val et: ET[ExerciseResponse[lav1.value.Value]] = for {
        response <- logResult(
          Symbol("createAndExercise"),
          submitAndWaitForTransactionTree(jwt, request),
        )
        exerciseResult <- either(exerciseResult(response))
        contracts <- either(contracts(response))
      } yield ExerciseResponse(exerciseResult, contracts)

      et.run
    }

  private def logResult[A](
      op: Symbol,
      fa: Grpc.EFuture[Grpc.Category.SubmitError, A],
  )(implicit
      lc: LoggingContextOf[InstanceUUID with RequestID]
  ): ET[A] = {
    val opName = op.name
    EitherT {
      fa.transformWith {
        case Failure(e) =>
          Future.successful(-\/(e match {
            case Grpc.StatusEnvelope(status) => GrpcError(status)
            case _ => InternalError(Some(op), e)
          }))
        case Success(-\/(e)) =>
          import Grpc.Category._
          val tagged = e.e match {
            case PermissionDenied => -\/(PermissionDenied)
            case InvalidArgument => \/-(InvalidArgument)
          }
          Future.successful(-\/(ClientError(tagged, e.message)))
        case Success(\/-(a)) =>
          logger.debug(s"$opName success: $a")
          Future.successful(\/-(a))
      }
    }
  }

  private def createCommand(
      input: CreateCommand[lav1.value.Record, TemplateId.RequiredPkg]
  ): lav1.commands.Command.Command.Create = {
    Commands.create(refApiIdentifier(input.templateId), input.payload)
  }

  private def exerciseCommand(
      input: ExerciseCommand[lav1.value.Value, ExerciseCommandRef]
  ): lav1.commands.Command.Command =
    input.reference match {
      case -\/((templateId, contractKey)) =>
        Commands.exerciseByKey(
          templateId = refApiIdentifier(templateId),
          contractKey = contractKey,
          choice = input.choice,
          argument = input.argument,
        )
      case \/-((templateId, contractId)) =>
        Commands.exercise(
          templateId = refApiIdentifier(templateId),
          contractId = contractId,
          choice = input.choice,
          argument = input.argument,
        )
    }

  private def createAndExerciseCommand(
      input: CreateAndExerciseCommand[lav1.value.Record, lav1.value.Value, TemplateId.RequiredPkg]
  ): lav1.commands.Command.Command.CreateAndExercise =
    Commands
      .createAndExercise(
        refApiIdentifier(input.templateId),
        input.payload,
        input.choice,
        input.argument,
      )

  private def submitAndWaitRequest(
      jwtPayload: JwtWritePayload,
      meta: Option[domain.CommandMeta],
      command: lav1.commands.Command.Command,
      commandKind: String,
  )(implicit
      lc: LoggingContextOf[InstanceUUID with RequestID]
  ): lav1.command_service.SubmitAndWaitRequest = {
    val commandId: lar.CommandId = meta.flatMap(_.commandId).getOrElse(uniqueCommandId())
    val actAs = meta.flatMap(_.actAs) getOrElse jwtPayload.submitter
    val readAs = meta.flatMap(_.readAs) getOrElse jwtPayload.readAs
    withEnrichedLoggingContext(
      label[lar.CommandId],
      "command_id" -> commandId.toString,
    )
      .run { implicit lc =>
        logger.info(
          s"Submitting $commandKind command"
        )
        Commands.submitAndWaitRequest(
          jwtPayload.ledgerId,
          jwtPayload.applicationId,
          commandId,
          actAs,
          readAs,
          command,
        )
      }
  }

  private def exactlyOneActiveContract(
      response: lav1.command_service.SubmitAndWaitForTransactionResponse
  ): Error \/ ActiveContract[lav1.value.Value] =
    activeContracts(response).flatMap {
      case Seq(x) => \/-(x)
      case xs @ _ =>
        -\/(
          InternalError(
            Some(Symbol("exactlyOneActiveContract")),
            s"Expected exactly one active contract, got: $xs",
          )
        )
    }

  private def activeContracts(
      response: lav1.command_service.SubmitAndWaitForTransactionResponse
  ): Error \/ ImmArraySeq[ActiveContract[lav1.value.Value]] =
    response.transaction
      .toRightDisjunction(
        InternalError(
          Some(Symbol("activeContracts")),
          s"Received response without transaction: $response",
        )
      )
      .flatMap(activeContracts)

  private def activeContracts(
      tx: lav1.transaction.Transaction
  ): Error \/ ImmArraySeq[ActiveContract[lav1.value.Value]] = {
    Transactions
      .allCreatedEvents(tx)
      .traverse(ActiveContract.fromLedgerApi(_))
      .leftMap(e => InternalError(Some(Symbol("activeContracts")), e.shows))
  }

  private def contracts(
      response: lav1.command_service.SubmitAndWaitForTransactionTreeResponse
  ): Error \/ List[Contract[lav1.value.Value]] =
    response.transaction
      .toRightDisjunction(
        InternalError(
          Some(Symbol("contracts")),
          s"Received response without transaction: $response",
        )
      )
      .flatMap(contracts)

  private def contracts(
      tx: lav1.transaction.TransactionTree
  ): Error \/ List[Contract[lav1.value.Value]] =
    Contract
      .fromTransactionTree(tx)
      .leftMap(e => InternalError(Some(Symbol("contracts")), e.shows))
      .map(_.toList)

  private def exerciseResult(
      a: lav1.command_service.SubmitAndWaitForTransactionTreeResponse
  ): Error \/ lav1.value.Value = {
    val result: Option[lav1.value.Value] = for {
      transaction <- a.transaction: Option[lav1.transaction.TransactionTree]
      exercised <- firstExercisedEvent(transaction): Option[lav1.event.ExercisedEvent]
      exResult <- exercised.exerciseResult: Option[lav1.value.Value]
    } yield exResult

    result.toRightDisjunction(
      InternalError(
        Some(Symbol("choiceArgument")),
        s"Cannot get exerciseResult from the first ExercisedEvent of gRPC response: ${a.toString}",
      )
    )
  }

  private def firstExercisedEvent(
      tx: lav1.transaction.TransactionTree
  ): Option[lav1.event.ExercisedEvent] = {
    val lookup: String => Option[lav1.event.ExercisedEvent] = id =>
      tx.eventsById.get(id).flatMap(_.kind.exercised)
    tx.rootEventIds.collectFirst(Function unlift lookup)
  }
}

object CommandService {
  sealed abstract class Error extends Product with Serializable
  final case class ClientError(
      id: Grpc.Category.PermissionDenied \/ Grpc.Category.InvalidArgument,
      message: String,
  ) extends Error
  final case class GrpcError(status: com.google.rpc.Status) extends Error
  final case class InternalError(id: Option[Symbol], error: Throwable) extends Error
  object InternalError {
    def apply(id: Option[Symbol], message: String): InternalError =
      InternalError(id, new Exception(message))
    def apply(id: Option[Symbol], error: Throwable): InternalError =
      InternalError(id, error)
  }

  private type ET[A] = EitherT[Future, Error, A]

  type ExerciseCommandRef = domain.ResolvedContractRef[lav1.value.Value]

  private val logger = ContextualizedLogger.get(classOf[CommandService])
}

package com.ialekseev.bob.syntax

import com.ialekseev.bob.Token
import com.ialekseev.bob.lexical.LexerToken
import scalaz._
import Scalaz._

//Top-Down Predictive Parsing for LL1 grammar (Recursive Descent technique)
class LL1SyntaxAnalyzer extends SyntaxAnalyzer with LL1SyntaxAnalysisState {

  //identifier
  private def parseIdentifier: ParserState[Parsed[Tree[Node]]] = current >>= {
    case Some(t@LexerToken(_: Token.Identifier, _)) => move >| (Terminal(t): Node).leaf.right
    case Some(LexerToken(_, offset)) => get[ParserStateInternal] >| Seq(ParserError(offset, "Unexpected Identifier")).left
    case None => get[ParserStateInternal].map(s => Seq(ParserError(s.tokens.last.offset, "Unexpected end")).left)
  }

  //NamespacePath ::= identifier {'.' identifier}
  private def parseNamespacePath(): ParserState[Parsed[Tree[Node]]] = {

    def parseDotPath(nodes: Seq[Tree[Node]]): ParserState[Parsed[Seq[Tree[Node]]]] = {
      current >>= {
        case Some(dot@LexerToken(_: Token.Delimiter.`.`.type, _)) => move >> parseIdentifier >>= {
          case \/-(identifier) => parseDotPath(nodes :+ (Terminal(dot): Node).leaf :+ identifier)
          case -\/(error) => error.left.point[ParserState]
        }
        case Some(_) =>  nodes.right.point[ParserState]
        case None => get[ParserStateInternal].map(s => Seq(ParserError(s.tokens.last.offset, "Unexpected end")).left)
      }
    }

    val parsed: ParserState[Parsed[Seq[Tree[Node]]]] = (for {
      identifier: Seq[Tree[Node]] <- EitherT.eitherT(liftToSeq(parseIdentifier))
      dotPath: Seq[Tree[Node]] <- EitherT.eitherT(parseDotPath(Seq.empty[Tree[Node]]))
    } yield identifier |+| dotPath).run

    parsed.map(either => {
      either.map(nodes => {
        Tree.Node(NonTerminal("NamespacePath"), nodes.toStream)
      })
    })
  }
}

package whsv26.Lexer

import scala.util.Try

object Token:
  case class FilePos(lineStart: Int = -1, lineEnd: Int = -1, posStart: Int = -1, posEnd: Int = -1)
  case class PhpToken(t: SimpleToken|ComplexToken, content: String, pos: FilePos = FilePos())

  def toPhpComplexToken(s: String) = Try({ ComplexToken.valueOf(s) }).toOption
  def toPhpSimpleToken(s: String) = s match
    case "(" => Some(SimpleToken.T_OPEN_ROUND_BRACKETS)
    case ")" => Some(SimpleToken.T_CLOSE_ROUND_BRACKETS)
    case "{" => Some(SimpleToken.T_OPEN_CURLY_BRACKETS)
    case "}" => Some(SimpleToken.T_CLOSE_CURLY_BRACKETS)
    case "[" => Some(SimpleToken.T_OPEN_SQUARE_BRACKETS)
    case "]" => Some(SimpleToken.T_CLOSE_SQUARE_BRACKETS)
    case "," => Some(SimpleToken.T_COMMA)
    case "." => Some(SimpleToken.T_DOT)
    case ":" => Some(SimpleToken.T_COLON)
    case ";" => Some(SimpleToken.T_SEMICOLON)
    case "=" => Some(SimpleToken.T_ASSIGN)
    case "?" => Some(SimpleToken.T_QUESTION)
    case "+" => Some(SimpleToken.T_PLUS)
    case "!" => Some(SimpleToken.T_EXCL)
    case _ => None

  enum SimpleToken(s: String):
    def content = s
    case T_OPEN_ROUND_BRACKETS extends SimpleToken("(")
    case T_CLOSE_ROUND_BRACKETS extends SimpleToken(")")
    case T_OPEN_CURLY_BRACKETS extends SimpleToken("{")
    case T_CLOSE_CURLY_BRACKETS extends SimpleToken("}")
    case T_OPEN_SQUARE_BRACKETS extends SimpleToken("[")
    case T_CLOSE_SQUARE_BRACKETS extends SimpleToken("]")
    case T_COMMA extends SimpleToken(",")
    case T_DOT extends SimpleToken(".")
    case T_COLON extends SimpleToken(":")
    case T_SEMICOLON extends SimpleToken(";")
    case T_ASSIGN extends SimpleToken("=")
    case T_QUESTION extends SimpleToken("?")
    case T_PLUS extends SimpleToken("+")
    case T_EXCL extends SimpleToken("!")

  enum ComplexToken:
    case T_ABSTRACT
    case T_AND_EQUAL
    case T_ARRAY
    case T_ARRAY_CAST
    case T_AS
    case T_BAD_CHARACTER
    case T_BOOLEAN_AND
    case T_BOOLEAN_OR
    case T_BOOL_CAST
    case T_BREAK
    case T_CALLABLE
    case T_CASE
    case T_CATCH
    case T_CLASS
    case T_CLASS_C
    case T_CLONE
    case T_CLOSE_TAG
    case T_COALESCE
    case T_COALESCE_EQUAL
    case T_COMMENT
    case T_CONCAT_EQUAL
    case T_CONST
    case T_CONSTANT_ENCAPSED_STRING
    case T_CONTINUE
    case T_CURLY_OPEN
    case T_DEC
    case T_DECLARE
    case T_DEFAULT
    case T_DIR
    case T_DIV_EQUAL
    case T_DNUMBER
    case T_DO
    case T_DOC_COMMENT
    case T_DOLLAR_OPEN_CURLY_BRACES
    case T_DOUBLE_ARROW
    case T_DOUBLE_CAST
    case T_DOUBLE_COLON
    case T_ECHO
    case T_ELLIPSIS
    case T_ELSE
    case T_ELSEIF
    case T_EMPTY
    case T_ENCAPSED_AND_WHITESPACE
    case T_ENDDECLARE
    case T_ENDFOR
    case T_ENDFOREACH
    case T_ENDIF
    case T_ENDSWITCH
    case T_ENDWHILE
    case T_END_HEREDOC
    case T_EVAL
    case T_EXIT
    case T_EXTENDS
    case T_FILE
    case T_FINAL
    case T_FINALLY
    case T_FN
    case T_FOR
    case T_FOREACH
    case T_FUNCTION
    case T_FUNC_C
    case T_GLOBAL
    case T_GOTO
    case T_HALT_COMPILER
    case T_IF
    case T_IMPLEMENTS
    case T_INC
    case T_INCLUDE
    case T_INCLUDE_ONCE
    case T_INLINE_HTML
    case T_INSTANCEOF
    case T_INSTEADOF
    case T_INTERFACE
    case T_INT_CAST
    case T_ISSET
    case T_IS_EQUAL
    case T_IS_GREATER_OR_EQUAL
    case T_IS_IDENTICAL
    case T_IS_NOT_EQUAL
    case T_IS_NOT_IDENTICAL
    case T_IS_SMALLER_OR_EQUAL
    case T_LINE
    case T_LIST
    case T_LNUMBER
    case T_LOGICAL_AND
    case T_LOGICAL_OR
    case T_LOGICAL_XOR
    case T_METHOD_C
    case T_MINUS_EQUAL
    case T_MOD_EQUAL
    case T_MUL_EQUAL
    case T_NAMESPACE
    case T_NEW
    case T_NS_C
    case T_NS_SEPARATOR
    case T_NUM_STRING
    case T_OBJECT_CAST
    case T_OBJECT_OPERATOR
    case T_NULLSAFE_OBJECT_OPERATOR
    case T_OPEN_TAG
    case T_OPEN_TAG_WITH_ECHO
    case T_OR_EQUAL
    case T_PAAMAYIM_NEKUDOTAYIM
    case T_PLUS_EQUAL
    case T_POW
    case T_POW_EQUAL
    case T_PRINT
    case T_PRIVATE
    case T_PROTECTED
    case T_PUBLIC
    case T_REQUIRE
    case T_REQUIRE_ONCE
    case T_RETURN
    case T_SL
    case T_SL_EQUAL
    case T_SPACESHIP
    case T_SR
    case T_SR_EQUAL
    case T_START_HEREDOC
    case T_STATIC
    case T_STRING
    case T_STRING_CAST
    case T_STRING_VARNAME
    case T_SWITCH
    case T_THROW
    case T_TRAIT
    case T_TRAIT_C
    case T_TRY
    case T_UNSET
    case T_UNSET_CAST
    case T_USE
    case T_VAR
    case T_VARIABLE
    case T_WHILE
    case T_WHITESPACE
    case T_XOR_EQUAL
    case T_YIELD
    case T_YIELD_FROM

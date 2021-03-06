
//----------------------------------------------------
// The following code was generated by CUP v0.10k TUM Edition 20050516
// Fri Jan 02 13:21:10 CET 2015
//----------------------------------------------------

package org.conqat.lib.simulink.builder;

import java.util.List;
import java.util.ArrayList;
import org.conqat.lib.commons.string.StringUtils;
import org.conqat.lib.commons.logging.ILogger;
import java.util.Collections;
import org.conqat.lib.commons.collections.PairList;

/** CUP v0.10k TUM Edition 20050516 generated parser.
  * @version Fri Jan 02 13:21:10 CET 2015
  */
public class MDLParser extends java_cup.runtime.lr_parser {

  /** Default constructor. */
  public MDLParser() {super();}

  /** Constructor which sets the default scanner. */
  public MDLParser(java_cup.runtime.Scanner s) {super(s);}

  /** Production table. */
  protected static final short _production_table[][] = 
    unpackFromStrings(new String[] {
    "\000\041\000\002\002\004\000\002\003\003\000\002\004" +
    "\004\000\002\004\003\000\002\005\010\000\002\005\007" +
    "\000\002\011\004\000\002\011\003\000\002\010\003\000" +
    "\002\010\003\000\002\006\004\000\002\006\005\000\002" +
    "\006\004\000\002\016\004\000\002\016\003\000\002\007" +
    "\004\000\002\007\004\000\002\007\003\000\002\012\003" +
    "\000\002\012\003\000\002\012\003\000\002\013\005\000" +
    "\002\013\003\000\002\015\005\000\002\015\004\000\002" +
    "\017\003\000\002\017\003\000\002\017\003\000\002\020" +
    "\003\000\002\020\003\000\002\021\002\000\002\014\005" +
    "\000\002\014\004" });

  /** Access to production table. */
  public short[][] production_table() {return _production_table;}

  /** Parse-action table. */
  protected static final short[][] _action_table = 
    unpackFromStrings(new String[] {
    "\000\061\000\004\014\007\001\002\000\006\002\000\014" +
    "\007\001\002\000\004\002\062\001\002\000\006\002\ufffe" +
    "\014\ufffe\001\002\000\004\005\010\001\002\000\010\006" +
    "\uffe3\014\uffe3\017\013\001\002\000\006\006\017\014\016" +
    "\001\002\000\010\002\uffe4\006\uffe4\014\uffe4\001\002\000" +
    "\010\002\uffe5\006\uffe5\014\uffe5\001\002\000\006\006\057" +
    "\014\016\001\002\000\006\006\ufff9\014\ufff9\001\002\000" +
    "\022\004\025\005\010\007\035\011\030\012\026\013\033" +
    "\014\034\017\036\001\002\000\012\002\uffe3\006\uffe3\014" +
    "\uffe3\017\013\001\002\000\006\006\ufff8\014\ufff8\001\002" +
    "\000\006\006\ufffa\014\ufffa\001\002\000\010\002\ufffc\006" +
    "\ufffc\014\ufffc\001\002\000\006\014\055\017\056\001\002" +
    "\000\004\017\054\001\002\000\020\004\uffed\010\uffed\012" +
    "\uffed\013\uffed\015\uffed\016\uffed\017\uffed\001\002\000\020" +
    "\004\uffef\010\uffef\012\uffef\013\uffef\015\uffef\016\uffef\017" +
    "\uffef\001\002\000\004\017\053\001\002\000\004\017\052" +
    "\001\002\000\006\006\ufff7\014\ufff7\001\002\000\010\006" +
    "\ufff0\011\050\014\ufff0\001\002\000\020\004\uffee\010\uffee" +
    "\012\uffee\013\uffee\015\uffee\016\uffee\017\uffee\001\002\000" +
    "\006\014\ufff3\017\ufff3\001\002\000\012\004\025\010\040" +
    "\012\026\013\033\001\002\000\006\006\ufff5\014\ufff5\001" +
    "\002\000\016\004\uffe3\010\043\012\uffe3\013\uffe3\015\042" +
    "\016\044\001\002\000\004\017\uffe1\001\002\000\016\004" +
    "\uffeb\010\uffeb\012\uffeb\013\uffeb\015\uffeb\016\uffeb\001\002" +
    "\000\010\004\uffe8\012\uffe8\013\uffe8\001\002\000\004\017" +
    "\uffe2\001\002\000\010\004\uffe7\012\uffe7\013\uffe7\001\002" +
    "\000\010\004\uffe6\012\uffe6\013\uffe6\001\002\000\010\004" +
    "\025\012\026\013\033\001\002\000\016\004\uffec\010\uffec" +
    "\012\uffec\013\uffec\015\uffec\016\uffec\001\002\000\004\017" +
    "\051\001\002\000\010\006\uffea\011\uffea\014\uffea\001\002" +
    "\000\010\006\uffe9\011\uffe9\014\uffe9\001\002\000\006\006" +
    "\ufff1\014\ufff1\001\002\000\006\006\ufff2\014\ufff2\001\002" +
    "\000\006\014\ufff4\017\ufff4\001\002\000\006\006\ufff6\014" +
    "\ufff6\001\002\000\012\002\uffe3\006\uffe3\014\uffe3\017\013" +
    "\001\002\000\006\006\ufffb\014\ufffb\001\002\000\010\002" +
    "\ufffd\006\ufffd\014\ufffd\001\002\000\004\002\001\001\002" +
    "\000\006\002\uffff\014\uffff\001\002" });

  /** Access to parse-action table. */
  public short[][] action_table() {return _action_table;}

  /** <code>reduce_goto</code> table. */
  protected static final short[][] _reduce_table = 
    unpackFromStrings(new String[] {
    "\000\061\000\010\003\004\004\003\005\005\001\001\000" +
    "\004\005\062\001\001\000\002\001\001\000\002\001\001" +
    "\000\002\001\001\000\006\020\010\021\011\001\001\000" +
    "\012\005\014\006\017\010\020\011\013\001\001\000\002" +
    "\001\001\000\002\001\001\000\010\005\014\006\017\010" +
    "\057\001\001\000\002\001\001\000\014\007\030\012\023" +
    "\014\026\015\031\016\022\001\001\000\006\020\021\021" +
    "\011\001\001\000\002\001\001\000\002\001\001\000\002" +
    "\001\001\000\002\001\001\000\002\001\001\000\002\001" +
    "\001\000\002\001\001\000\002\001\001\000\002\001\001" +
    "\000\002\001\001\000\002\001\001\000\002\001\001\000" +
    "\002\001\001\000\006\012\040\013\036\001\001\000\002" +
    "\001\001\000\006\017\045\021\044\001\001\000\002\001" +
    "\001\000\002\001\001\000\002\001\001\000\002\001\001" +
    "\000\002\001\001\000\002\001\001\000\004\012\046\001" +
    "\001\000\002\001\001\000\002\001\001\000\002\001\001" +
    "\000\002\001\001\000\002\001\001\000\002\001\001\000" +
    "\002\001\001\000\002\001\001\000\006\020\060\021\011" +
    "\001\001\000\002\001\001\000\002\001\001\000\002\001" +
    "\001\000\002\001\001" });

  /** Access to <code>reduce_goto</code> table. */
  public short[][] reduce_table() {return _reduce_table;}

  /** Instance of action encapsulation class. */
  protected CUP$MDLParser$actions action_obj;

  /** Action encapsulation object initializer. */
  protected void init_actions()
    {
      action_obj = new CUP$MDLParser$actions(this);
    }

  /** Invoke a user supplied parse action. */
  public java_cup.runtime.Symbol do_action(
    int                        act_num,
    java_cup.runtime.lr_parser parser,
    java.util.Stack            stack,
    int                        top)
    throws java.lang.Exception
  {
    /* call code in generated class */
    return action_obj.CUP$MDLParser$do_action(act_num, parser, stack, top);
  }

  /** Indicates start state. */
  public int start_state() {return 0;}
  /** Indicates start production. */
  public int start_production() {return 0;}

  /** <code>EOF</code> Symbol index. */
  public int EOF_sym() {return 0;}

  /** <code>error</code> Symbol index. */
  public int error_sym() {return 1;}


 

  private ILogger logger;

	public MDLParser(java_cup.runtime.Scanner scanner, ILogger logger) {
		super(scanner);
		this.logger = logger;
	}

	public void debug(String message) {
		if (logger != null) {
			logger.debug(message);
		} else {
			System.out.println(message);
		}
	}

	public void report_error(String message, Object info) {
		MDLParserException exception = createException(message, info);

		if (logger != null) {
			logger.warn(exception.getMessage());
		} else {
			System.err.println(exception.getMessage());
		}
	}

	public void report_fatal_error(String message, Object info)
			throws MDLParserException {
		throw createException(message, info);
	}

	public MDLParserException createException(String message, Object info) {
		if (info instanceof java_cup.runtime.Symbol) {
			java_cup.runtime.Symbol symbol = ((java_cup.runtime.Symbol) info);

			int line = symbol.left + 1;
			int column = symbol.right + 1;

			return new MDLParserException(message + " at line: " + line
					+ ", column: " + column, line, column);
		}

		return new MDLParserException(message);
	}
	
	static class Parameter {
	  public final String name;
	  public final String value;
	  
	  public Parameter (String name, String value) {
	    this.name=name;
	    this.value=value;
	  }
	}

}

/** Cup generated class to encapsulate user supplied action code.*/
class CUP$MDLParser$actions {
  private final MDLParser parser;

  /** Constructor */
  CUP$MDLParser$actions(MDLParser parser) {
    this.parser = parser;
  }

  /** Method with the actual generated action code. */
  public final java_cup.runtime.Symbol CUP$MDLParser$do_action(
    int                        CUP$MDLParser$act_num,
    java_cup.runtime.lr_parser CUP$MDLParser$parser,
    java.util.Stack            CUP$MDLParser$stack,
    int                        CUP$MDLParser$top)
    throws java.lang.Exception
    {
      /* Symbol object for return from actions */
      java_cup.runtime.Symbol CUP$MDLParser$result;

      /* select the action based on the action number */
      switch (CUP$MDLParser$act_num)
        {
          /*. . . . . . . . . . . . . . . . . . . .*/
          case 32: // Array ::= LBRACK RBRACK 
            {
              String RESULT = null;
		 RESULT = "[]"; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(10/*Array*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 31: // Array ::= LBRACK LiteralList RBRACK 
            {
              String RESULT = null;
		int lleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left;
		int lright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).right;
		List<String> l = (List<String>)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).value;
		 RESULT = "["+StringUtils.concat(l, ", ")+"]"; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(10/*Array*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 30: // Empty ::= 
            {
              Object RESULT = null;

              CUP$MDLParser$result = new java_cup.runtime.Symbol(15/*Empty*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 29: // OptNewline ::= Empty 
            {
              Object RESULT = null;

              CUP$MDLParser$result = new java_cup.runtime.Symbol(14/*OptNewline*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 28: // OptNewline ::= NEWLINE 
            {
              Object RESULT = null;

              CUP$MDLParser$result = new java_cup.runtime.Symbol(14/*OptNewline*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 27: // OptSeperator ::= Empty 
            {
              Object RESULT = null;

              CUP$MDLParser$result = new java_cup.runtime.Symbol(13/*OptSeperator*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 26: // OptSeperator ::= SEMICOLON 
            {
              Object RESULT = null;

              CUP$MDLParser$result = new java_cup.runtime.Symbol(13/*OptSeperator*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 25: // OptSeperator ::= COMMA 
            {
              Object RESULT = null;

              CUP$MDLParser$result = new java_cup.runtime.Symbol(13/*OptSeperator*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 24: // StringList ::= STRING_LITERAL NEWLINE 
            {
              StringBuilder RESULT = null;
		int sleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left;
		int sright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).right;
		String s = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).value;
		 StringBuilder list = new StringBuilder();
                  list.append(s);
                  RESULT=list; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(11/*StringList*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 23: // StringList ::= StringList STRING_LITERAL NEWLINE 
            {
              StringBuilder RESULT = null;
		int listleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).left;
		int listright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).right;
		StringBuilder list = (StringBuilder)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).value;
		int sleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left;
		int sright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).right;
		String s = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).value;
		 list.append(s);
                  RESULT = list; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(11/*StringList*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 22: // LiteralList ::= Literal 
            {
              List<String> RESULT = null;
		int lleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int lright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		String l = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 ArrayList<String> list = new ArrayList<String>();
             list.add(l);
             RESULT = list; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(9/*LiteralList*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 21: // LiteralList ::= LiteralList OptSeperator Literal 
            {
              List<String> RESULT = null;
		int listleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).left;
		int listright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).right;
		List<String> list = (List<String>)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).value;
		int lleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int lright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		String l = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 list.add(l);
                RESULT = list; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(9/*LiteralList*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 20: // Literal ::= BOOLEAN_LITERAL 
            {
              String RESULT = null;
		int lleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int lright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		String l = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 RESULT =l; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(8/*Literal*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 19: // Literal ::= FLOAT_LITERAL 
            {
              String RESULT = null;
		int lleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int lright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		String l = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 RESULT = l; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(8/*Literal*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 18: // Literal ::= INT_LITERAL 
            {
              String RESULT = null;
		int lleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int lright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		String l = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 RESULT = l; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(8/*Literal*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 17: // Value ::= StringList 
            {
              String RESULT = null;
		int sleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int sright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		StringBuilder s = (StringBuilder)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 RESULT = s.toString();  
              CUP$MDLParser$result = new java_cup.runtime.Symbol(5/*Value*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 16: // Value ::= Array NEWLINE 
            {
              String RESULT = null;
		int aleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left;
		int aright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).right;
		String a = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).value;
		 RESULT = a; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(5/*Value*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 15: // Value ::= Literal NEWLINE 
            {
              String RESULT = null;
		int lleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left;
		int lright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).right;
		String l = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).value;
		 RESULT = l; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(5/*Value*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 14: // IdentifierList ::= IDENTIFIER 
            {
              StringBuilder RESULT = null;
		int ileft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int iright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		String i = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 StringBuilder list = new StringBuilder(); list.append(i); RESULT = list; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(12/*IdentifierList*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 13: // IdentifierList ::= IdentifierList IDENTIFIER 
            {
              StringBuilder RESULT = null;
		int listleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left;
		int listright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).right;
		StringBuilder list = (StringBuilder)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).value;
		int ileft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int iright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		String i = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 list.append(" "); list.append(i); RESULT = list; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(12/*IdentifierList*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 12: // Parameter ::= IDENTIFIER NEWLINE 
            {
              MDLParser.Parameter RESULT = null;
		int ileft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left;
		int iright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).right;
		String i = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).value;
		 RESULT = new MDLParser.Parameter(i, ""); 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(4/*Parameter*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 11: // Parameter ::= IDENTIFIER IdentifierList NEWLINE 
            {
              MDLParser.Parameter RESULT = null;
		int i1left = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).left;
		int i1right = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).right;
		String i1 = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).value;
		int lleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left;
		int lright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).right;
		StringBuilder l = (StringBuilder)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).value;
		 RESULT = new MDLParser.Parameter(i1, l.toString()); 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(4/*Parameter*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 10: // Parameter ::= IDENTIFIER Value 
            {
              MDLParser.Parameter RESULT = null;
		int ileft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left;
		int iright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).right;
		String i = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).value;
		int aleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int aright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		String a = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 RESULT = new MDLParser.Parameter(i, a);  
              CUP$MDLParser$result = new java_cup.runtime.Symbol(4/*Parameter*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 9: // SectionOrParameter ::= Parameter 
            {
              Object RESULT = null;
		int pleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int pright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		MDLParser.Parameter p = (MDLParser.Parameter)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 RESULT = p; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(6/*SectionOrParameter*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 8: // SectionOrParameter ::= Section 
            {
              Object RESULT = null;
		int bleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int bright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		MDLSection b = (MDLSection)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 RESULT = b; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(6/*SectionOrParameter*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 7: // SectionOrParameterList ::= SectionOrParameter 
            {
              List<Object> RESULT = null;
		int eleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int eright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		Object e = (Object)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 ArrayList<Object> list = 
                              new ArrayList<Object>();
                           list.add(e);
                           RESULT = list; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(7/*SectionOrParameterList*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 6: // SectionOrParameterList ::= SectionOrParameterList SectionOrParameter 
            {
              List<Object> RESULT = null;
		int lleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left;
		int lright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).right;
		List<Object> l = (List<Object>)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).value;
		int eleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int eright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		Object e = (Object)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 l.add(e); 
                           RESULT = l; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(7/*SectionOrParameterList*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 5: // Section ::= IDENTIFIER LBRACE OptNewline RBRACE OptNewline 
            {
              MDLSection RESULT = null;
		int ileft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-4)).left;
		int iright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-4)).right;
		String i = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-4)).value;
		 RESULT = new MDLSection(i, ileft+1); 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(3/*Section*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-4)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 4: // Section ::= IDENTIFIER LBRACE OptNewline SectionOrParameterList RBRACE OptNewline 
            {
              MDLSection RESULT = null;
		int ileft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-5)).left;
		int iright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-5)).right;
		String i = (String)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-5)).value;
		int lleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).left;
		int lright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).right;
		List<Object> l = (List<Object>)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-2)).value;
		 ArrayList<MDLSection> sections = new ArrayList<MDLSection>();
             PairList<String, String> parameters = new PairList<String, String>();
               
             for (Object o : l) {
                if (o instanceof MDLSection) {
                  sections.add((MDLSection)o);
                } else if (o instanceof MDLParser.Parameter) {
                  MDLParser.Parameter param = (MDLParser.Parameter)o;
                  parameters.add(param.name, param.value);
                } else {
                  throw new IllegalStateException("Parser implementation error");
                }
             }
          
             RESULT = new MDLSection(i, sections, parameters, ileft+1); 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(3/*Section*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-5)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 3: // SectionList ::= Section 
            {
              List<MDLSection> RESULT = null;
		int bleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int bright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		MDLSection b = (MDLSection)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 ArrayList<MDLSection> list = new ArrayList<MDLSection>();
                list.add(b);
                RESULT = list; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(2/*SectionList*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 2: // SectionList ::= SectionList Section 
            {
              List<MDLSection> RESULT = null;
		int lleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left;
		int lright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).right;
		List<MDLSection> l = (List<MDLSection>)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).value;
		int bleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int bright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		MDLSection b = (MDLSection)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 l.add(b);
                 RESULT = l; 
              CUP$MDLParser$result = new java_cup.runtime.Symbol(2/*SectionList*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 1: // File ::= SectionList 
            {
              MDLSection RESULT = null;
		int lleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left;
		int lright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right;
		List<MDLSection> l = (List<MDLSection>)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).value;
		 RESULT =  new MDLSection("", l, new PairList<String, String>(), -1);  
              CUP$MDLParser$result = new java_cup.runtime.Symbol(1/*File*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          return CUP$MDLParser$result;

          /*. . . . . . . . . . . . . . . . . . . .*/
          case 0: // $START ::= File EOF 
            {
              Object RESULT = null;
		int start_valleft = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left;
		int start_valright = ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).right;
		MDLSection start_val = (MDLSection)((java_cup.runtime.Symbol) CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).value;
		RESULT = start_val;
              CUP$MDLParser$result = new java_cup.runtime.Symbol(0/*$START*/, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-1)).left, ((java_cup.runtime.Symbol)CUP$MDLParser$stack.elementAt(CUP$MDLParser$top-0)).right, RESULT);
            }
          /* ACCEPT */
          CUP$MDLParser$parser.done_parsing();
          return CUP$MDLParser$result;

          /* . . . . . .*/
          default:
            throw new Exception(
               "Invalid action number found in internal parse table");

        }
    }
}


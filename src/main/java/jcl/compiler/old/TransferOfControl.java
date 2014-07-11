package jcl.compiler.old;

import jcl.compiler.old.exception.GoException;
import jcl.compiler.old.exception.ReturnFromException;
import jcl.compiler.old.exception.TRFException;
import jcl.compiler.old.exception.ThrowException;

import java.util.Stack;

/**
 * TransferOfControl.java
 * <p>
 * Created on February 28, 2007, 5:11 AM
 * <p>
 * This class is responsible for the runtime management of all of the tranfsers of control.
 * It maintains a runtime TOCMgmt stack that stores the current state of the control transfers
 * at any given time and allows the mgmt of this stack.
 */
public class TransferOfControl {

	/**
	 * This is simply a struct that holds the information about an instance of a control transfer that will be held on the TOCMgmt stack.
	 */
	private static class TOCRecord {

		public String type;         //The type {Block, TagBody, Catch} of the TOCRecord.
		public Object value;        /*This is the tag or symbol that the corresponding type is allowed to process
		For tagbody, since it can have multiple labels, it will be a list of objects holding
        all of the corresponding label values.  */

		public boolean valid;       //Whether this TOCRecord is still in scope or not

		/**
		 * Creates a new instance of the TOCRecord with it being valid by default
		 */
		public TOCRecord(String type, Object value) {
			this.type = type;
			this.value = value;
			this.valid = true;
		}
	}

	//These are the constants used to coordinate for the type of TOCRecord that is stored on the TOCMgmt stack
	//They are used both by the callers (in ICG and within this class)
	public static final String BLOCK = "Block";
	public static final String TAGBODY = "Tagbody";
	public static final String CATCH = "Catch";
	private static TransferOfControl me = new TransferOfControl();     //This is the only instance of TOC that will ever be create
	/**
	 * This is a runtime stack that manages all of the runtime transfer of control points.
	 * During compilation, all TOC types establish the code to manage the pushing and popping
	 * of TOCRecords to/from the stack. The push will always occur immediately in the code and
	 * the pop will always occur in the finally block of the corresponding TOC.
	 */
	private static Stack<TOCRecord> TOCMgmt;
	/**
	 * If the unwind-protect block is entered because an exception was thrown
	 * (ie...Go, Throw, or Return-from occured) and not from regular control flow returns,
	 * it must store and rethrow the corresponding exception after it has executed the cleanup form.
	 * This is the exception that will be rethrown at the end of the cleanup form (if one is going to be rethrown at all).
	 */
	private static Throwable returnException;

	/**
	 * This is a private constructor to support the creation of this singleton class
	 */
	private TransferOfControl() {
		TransferOfControl.TOCMgmt = new Stack<TOCRecord>();
		TransferOfControl.returnException = null;
	}

	/**
	 * This method is a run-time registration mechanism for all of the TOC special operators.
	 * All TOC bodies (block, tagbody, and catch) immediataly call this method at runtime when thier scope is entered.
	 * They store thier type of TOC and what tag values or symbols they are authorized to process in the event a corresponding event
	 * (go, throw, return-from) occurs.
	 */
	public static void addTOCRecord(String type, Object value) {
		TOCMgmt.push(new TOCRecord(type, value));
	}

	/**
	 * All of the TOC special operators will generate code in their finally block to call this method.
	 * This is necessary clearnup in order to ensure that the runtime TOC stack is correctly maintained.
	 * Since the TOCMgmt stack can have heterogeneous TOC types on the stack at any given time,
	 * the callers will pass the type and this method will find and remove the first instance of that corresponding type on the stack.
	 */
	public static void popTOCRecord() {
		TOCMgmt.pop();
	}

	/**
	 * This method is only called if one of the TOC types actually caught an exception; otherwise,
	 * the finally block of the corresponding code will be executed and the TOCRecord will simply
	 * be removed from the TOCMgmt stack. If a handler does catch the exception,
	 * it will call this method with the corresponding TOC type and exception.
	 * This method will find the first instance of the corresponding type on the stack,
	 * verify that the Exception is of the correct type and if so compare the value with the
	 * value that this already on the stack (ie...what tags/symbols this exception is allowed to process)
	 * and return the result of the comparison. If the compare fails, the caller will remove himself
	 * from the stack and rethrow the exception; otherwise, they will remove themselves from the stack,
	 * handle the exception, and continue as normal.
	 */
	@SuppressWarnings("unchecked")
	public static Object isMine(Throwable ex) {
		TOCRecord current;          //The first record on the stack
		Object tempValue;           //Return value of exception
		Class tempCatch, tempThrow; //Classes for Catchtag and Thrown exception

		TransferOfControl.setReturnException(null);

		//If the stack is empty, the caller shouldn't have made this call in the first place
		if (TOCMgmt.empty()) {
			return null;
		}

		current = TOCMgmt.peek();

		//If the record is tagged as invalid, it's an automatic fail
		if (!current.valid) {
			return null;
		}

		if (matches(current, ex)) {
			//If there is a potential return value and this is my exception, return it
			if (ex instanceof TRFException) {
				tempValue = ((TRFException) ex).getValue();
				if (tempValue != null) {
					return tempValue;
				} else {
					return new Object();
				}
			}
			//Otherwise (for 'go'), just return the tag itself that was caught
			return ((GoException) ex).getTag().toString();

		} else if ((ex instanceof Throwable)
				&& (current.type.compareTo(CATCH) == 0)) {
			tempThrow = ex.getClass();
			if (tempThrow != null) {
				if (((Class) current.value).isAssignableFrom(tempThrow)) {
					return ex;
				} else {
					return null;
				}
			} else {
				return null;
			}
		} else //Getting here means this is not a TOC exception or the values don't match up
		{
			return null;
		}
	}

	private static boolean matches(TOCRecord current, Throwable ex) {
		if ((ex instanceof ThrowException)
				&& (current.type.compareTo(CATCH) == 0)
				&& (current.value == ((ThrowException) ex).getTag())) {
			return true;

		} else if ((ex instanceof ReturnFromException)
				&& (current.type.compareTo(BLOCK) == 0)
				&& (current.value == ((ReturnFromException) ex).getTag())) {
			return true;

		} else if ((ex instanceof GoException)
				&& (current.type.compareTo(TAGBODY) == 0)
				&& (current.value.toString().contains(((GoException) ex).getTag().toString()))) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Immediately after the finally block from the unwind-protect form begins execution, this method is called.
	 * This method will traverse up the TOCMgmt stack and disable every single TOCRecord until it finds the first instance of a
	 * TOCRecord that matches the current returnException value. All other methods (such as isMine) will always check the valid
	 * bit before validating an entry in the TOCMgmt stack. This will ensure that the cleanup form from unwind-protect is unable
	 * to transfer back into anything that was within scope during the protected form execution.
	 * If this does occur, when the exception handler from the protected form tries to handle the exception,
	 * it will get a false from isMine and therefore, throw the exception right back up the runtime stack.
	 */
	public static void disableExitPoints(Throwable ex) {
		Stack<TOCRecord> tempStack = new Stack<TOCRecord>();     //Stack used to simply optimize a normally recursive call
		TOCRecord tempRecord;
		boolean found = false;

		while ((!found) && (!TOCMgmt.empty())) {
			tempRecord = TOCMgmt.pop();
			if (matches(tempRecord, ex)) {
				found = true;
			} else {
				tempRecord.valid = false;
			}
			tempStack.push(tempRecord);
		}

		//Rebuild the original stack now
		while (!tempStack.empty()) {
			TOCMgmt.push(tempStack.pop());
		}
	}

	/**
	 * This simply sets the returnException value in case the TOC occured because of an explicit control transfer.
	 * This value will be utilized later to throw back up after the TOC has executed finally code.
	 * If another control transfer occurs in the finally code, the value will be ignored and never used.
	 * It will get set back to null every time a processReturnException call is made.
	 */
	public static void setReturnException(Throwable ex) {
		returnException = ex;
	}

	/**
	 * If an exception was caught (and was not handled by the current TOC), it would have been stored as the returnException.
	 * If this was the case, once the finally code of the TOC is completed, this will be the last call made before going on to
	 * the continuation block. If there is a valid exception, it will simply be rethrown; otherwise, control will return to the
	 * current TOC, and they will proceed to their respective continuation block.
	 */
	public static void processReturnException() throws Throwable {
		Throwable temp = returnException;
		returnException = null;

		if (temp != null) {
			throw temp;
		} else {
			return;
		}
	}
}

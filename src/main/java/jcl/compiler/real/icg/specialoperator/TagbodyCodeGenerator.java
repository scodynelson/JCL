package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.structs.lists.ListStruct;
import jcl.structs.lists.NullStruct;
import jcl.structs.symbols.SymbolStruct;
import org.objectweb.asm.Label;

import java.util.Stack;

public class TagbodyCodeGenerator {

	public static int tagCounter;

	public static void genCodeTagbody(final IntermediateCodeGenerator icg, ListStruct list) {
		String tagbodyName;

		final Label startTryBlock = new Label();                //The start of the try block
		final Label catchBlock = new Label();                   //The start of the catch block
		final Label continueBlock = new Label();                //Subsequent code after the try/catch construct
		final Label ifBlock = new Label();                      //If block executed if this catches someone else's excepton
		final Label elseBlock = new Label();                    //If the exception is caught block

        /* Skip past the TAGBODY symbol. */
		list = list.getRest();

        /* Read all the tags within the TAGBODY form. */
		final Stack<TagbodyLabel> tagStack = tagbodyReadLabels(list);
		icg.tagbodyStack.push(tagStack);

        /* Create a string with all of the int index values from all of the labels in this tagbody
         * This will be used to setup the TOCMgmt stack record
         */
		int size = tagStack.size();                           //Size of the tagbodylabel stack
		String allTagbodyLabels = "";       //A delim string of all the valid ints in the tagbody
		final String DELIM = "\t";      //The delimiter used for the string
		while (size-- > 0) {
			allTagbodyLabels = allTagbodyLabels + tagStack.get(size).index + DELIM;
		}

		//Set up the TOC management record
		// ... ,
		icg.emitter.emitLdc(allTagbodyLabels);
		// ..., tagBodyLabels
		icg.emitter.emitGetstatic("lisp/system/TransferOfControl", "TAGBODY", "Ljava/lang/String;");
		// ..., tagBodyLabels, TAGBODY
		icg.emitter.emitSwap();
		// ... , TAGBODY, tagBodyLabels
		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "addTOCRecord", "(Ljava/lang/String;Ljava/lang/Object;)", "V", false);

        /* Invoke the ICG for each non-tag statement within the TAGBODY form. */
		icg.emitter.visitMethodLabel(startTryBlock);
		// +0
		while (!list.equals(NullStruct.INSTANCE)) {
			final Object obj = list.getFirst();

            /* If the car of the list is a Symbol, then its a tag, which means
             * a label needs to be emitted. Otherwise call the ICG on the car of
             * the list. */
			if (obj instanceof SymbolStruct) {
				final SymbolStruct<?> sym = (SymbolStruct) obj;
				// find the symbol in the tagbody stack
				final TagbodyLabel tbl = findTagbodyInStack(icg.tagbodyStack, (SymbolStruct) obj);
				icg.emitter.visitMethodLabel(tbl.label);
			} else {
				icg.icgMainLoop(obj);
				icg.emitter.emitPop(); // Throws away the results of any forms in the tag body
			}
			list = list.getRest();
		}

        /* If execution makes it all the way through with no exception then skip
         * over the exception handler. */
		// +0
		icg.emitter.emitGoto(continueBlock);

		icg.emitter.visitMethodLabel(catchBlock);
		// ..., excep
		icg.emitter.emitDup();
		// ..., excep, excep

		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "isMine", "(Ljava/lang/Throwable;)", "Ljava/lang/Object;", false);

		// ..., excep, result
		icg.emitter.emitDup();
		// ..., excep, result, result
		icg.emitter.emitIfnull(ifBlock);
		icg.emitter.emitGoto(elseBlock);
		//If block start
		icg.emitter.visitMethodLabel(ifBlock);
		// ..., excep, result
		icg.emitter.emitPop();
		// ..., excep
		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "setReturnException", "(Ljava/lang/Throwable;)", "V", false);
		// ...,
		icg.emitter.emitGoto(continueBlock);
		//If block end

		//Else block start
		icg.emitter.visitMethodLabel(elseBlock);
		// ..., excep, result
		icg.emitter.emitSwap();
		// ..., result, excep
		icg.emitter.emitPop();
		// ..., result

		icg.emitter.emitInvokevirtual("java/lang/Object", "toString", "()", "Ljava/lang/String;", false);
		// ..., resultString

		icg.emitter.emitInvokestatic("java/lang/Integer", "parseInt", "(Ljava/lang/String;)", "I", false);

		// +1 - int
		// create a lookup switch for the labels
		final int tagsSize = tagStack.size();
		final int[] tagNumbers = new int[tagsSize];
		final Label[] tagLabels = new Label[tagsSize];
		for (int index = 0; index < tagsSize; index++) {
			tagNumbers[index] = tagStack.get(index).index;
			tagLabels[index] = tagStack.get(index).label;
		}
		final Label defaultLabel = new Label();
		// now create the tableswitch
		// +1 - int
		icg.emitter.emitTableswitch(tagNumbers[0], tagNumbers[tagsSize - 1], defaultLabel, tagLabels);
		// +0
		/* Throw another exception to the most enclosing TAGBODY. */
		icg.emitter.visitMethodLabel(defaultLabel);
		icg.emitter.emitGoto(continueBlock);
		//Else block end

        /* Emit the post-exception handler label, and pop the tag stack from
         * 'tagbodyStack'. */
		icg.emitter.visitMethodLabel(continueBlock);

        /* TAGBODY always returns NIL, so put a NIL on the stack to be
         * returned. */
		icg.emitter.emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");

		//This is compilation only code
		icg.tagbodyStack.pop();
		icg.emitter.visitTryCatchBlock(startTryBlock, catchBlock, catchBlock, "java/lang/Throwable");

		//Here is the finally code
		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "popTOCRecord", "()", "V", false);
		icg.emitter.emitInvokestatic("lisp/system/TransferOfControl", "processReturnException", "()", "V", false);
	}

	/**
	 * Reads all the tags in the TAGBODY form and inserts them into a stack
	 * which is returned. Its necessary to do this first since a GO can be
	 * executed for a tag declared later in the form.
	 */
	public static class TagbodyLabel {

		final SymbolStruct<?> symbol;
		final Label label;
		final int index = tagCounter++;

		TagbodyLabel(final SymbolStruct<?> symbol, final Label label) {
			this.symbol = symbol;
			this.label = label;
		}
	}

	private static Stack<TagbodyLabel> tagbodyReadLabels(ListStruct list) {
		final Stack<TagbodyLabel> tagStack = new Stack<>();

		while (!list.equals(NullStruct.INSTANCE)) {
			final Object obj = list.getFirst();
			if (obj instanceof SymbolStruct) {
				// Insert the tag into the stack.
				tagStack.push(new TagbodyLabel((SymbolStruct) obj, new Label()));
			}
			list = list.getRest();
		}

		return tagStack;
	}

	private static TagbodyLabel findTagbodyBySymbol(final Stack<TagbodyLabel> stack, final SymbolStruct<?> symbol) {
		int size = stack.size();
		while (size-- > 0) {
			final TagbodyLabel tbl = stack.get(size);
			if (tbl.symbol.equals(symbol)) {
				return tbl;
			}
		}
		return null;
	}

	public static TagbodyLabel findTagbodyInStack(final Stack<Stack<TagbodyLabel>> stack, final SymbolStruct<?> symbol) {
		int size = stack.size();
		while (size-- > 0) {
			final Stack<TagbodyLabel> tbs = stack.get(size);
			final TagbodyLabel tbl = findTagbodyBySymbol(tbs, symbol);
			if (tbl != null) {
				return tbl;
			}
		}
		return null;
	}
}

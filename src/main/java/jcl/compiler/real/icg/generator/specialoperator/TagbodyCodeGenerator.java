package jcl.compiler.real.icg.generator.specialoperator;

import java.util.Stack;

import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.springframework.stereotype.Component;

@Component
public class TagbodyCodeGenerator implements CodeGenerator<ListStruct> {

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		String tagbodyName;

		final Label startTryBlock = new Label();                //The start of the try block
		final Label catchBlock = new Label();                   //The start of the catch block
		final Label continueBlock = new Label();                //Subsequent code after the try/catch construct
		final Label ifBlock = new Label();                      //If block executed if this catches someone else's excepton
		final Label elseBlock = new Label();                    //If the exception is caught block

        /* Skip past the TAGBODY symbol. */
		ListStruct restOfList = input.getRest();

        /* Read all the tags within the TAGBODY form. */
		final Stack<TagbodyLabel> tagStack = tagbodyReadLabels(restOfList, classBuilder);
		classBuilder.getTagbodyStack().push(tagStack);

        /* Create a string with all of the int index values from all of the labels in this tagbody
         * This will be used to setup the TOCMgmt stack record
         */
		int size = tagStack.size();                           //Size of the tagbodylabel stack
		String allTagbodyLabels = "";       //A delim string of all the valid ints in the tagbody
		final String DELIM = "\t";      //The delimiter used for the string
		while (size-- > 0) {
			allTagbodyLabels = allTagbodyLabels + tagStack.get(size).getIndex() + DELIM;
		}

		//Set up the TOC management record
		// ... ,
		classBuilder.getEmitter().emitLdc(allTagbodyLabels);
		// ..., tagBodyLabels
		classBuilder.getEmitter().emitGetstatic("lisp/system/TransferOfControl", "TAGBODY", "Ljava/lang/String;");
		// ..., tagBodyLabels, TAGBODY
		classBuilder.getEmitter().emitSwap();
		// ... , TAGBODY, tagBodyLabels
		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "addTOCRecord", "(Ljava/lang/String;Ljava/lang/Object;)", "V", false);

        /* Invoke the ICG for each non-tag statement within the TAGBODY form. */
		classBuilder.getEmitter().visitMethodLabel(startTryBlock);
		// +0
		while (!restOfList.equals(NullStruct.INSTANCE)) {
			final Object obj = restOfList.getFirst();

            /* If the car of the list is a Symbol, then its a tag, which means
             * a label needs to be emitted. Otherwise call the ICG on the car of
             * the list. */
			if (obj instanceof SymbolStruct) {
				final SymbolStruct<?> sym = (SymbolStruct) obj;
				// find the symbol in the tagbody stack
				final TagbodyLabel tbl = findTagbodyInStack(classBuilder.getTagbodyStack(), (SymbolStruct) obj);
				classBuilder.getEmitter().visitMethodLabel(tbl.getLabel());
			} else {
				codeGenerator.icgMainLoop(obj, classBuilder);
				classBuilder.getEmitter().emitPop(); // Throws away the results of any forms in the tag body
			}
			restOfList = restOfList.getRest();
		}

        /* If execution makes it all the way through with no exception then skip
         * over the exception handler. */
		// +0
		classBuilder.getEmitter().emitGoto(continueBlock);

		classBuilder.getEmitter().visitMethodLabel(catchBlock);
		// ..., excep
		classBuilder.getEmitter().emitDup();
		// ..., excep, excep

		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "isMine", "(Ljava/lang/Throwable;)", "Ljava/lang/Object;", false);

		// ..., excep, result
		classBuilder.getEmitter().emitDup();
		// ..., excep, result, result
		classBuilder.getEmitter().emitIfnull(ifBlock);
		classBuilder.getEmitter().emitGoto(elseBlock);
		//If block start
		classBuilder.getEmitter().visitMethodLabel(ifBlock);
		// ..., excep, result
		classBuilder.getEmitter().emitPop();
		// ..., excep
		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "setReturnException", "(Ljava/lang/Throwable;)", "V", false);
		// ...,
		classBuilder.getEmitter().emitGoto(continueBlock);
		//If block end

		//Else block start
		classBuilder.getEmitter().visitMethodLabel(elseBlock);
		// ..., excep, result
		classBuilder.getEmitter().emitSwap();
		// ..., result, excep
		classBuilder.getEmitter().emitPop();
		// ..., result

		classBuilder.getEmitter().emitInvokevirtual("java/lang/Object", "toString", "()", "Ljava/lang/String;", false);
		// ..., resultString

		classBuilder.getEmitter().emitInvokestatic("java/lang/Integer", "parseInt", "(Ljava/lang/String;)", "I", false);

		// +1 - int
		// create a lookup switch for the labels
		final int tagsSize = tagStack.size();
		final int[] tagNumbers = new int[tagsSize];
		final Label[] tagLabels = new Label[tagsSize];
		for (int index = 0; index < tagsSize; index++) {
			tagNumbers[index] = tagStack.get(index).getIndex();
			tagLabels[index] = tagStack.get(index).getLabel();
		}
		final Label defaultLabel = new Label();
		// now create the tableswitch
		// +1 - int
		classBuilder.getEmitter().emitTableswitch(tagNumbers[0], tagNumbers[tagsSize - 1], defaultLabel, tagLabels);
		// +0
		/* Throw another exception to the most enclosing TAGBODY. */
		classBuilder.getEmitter().visitMethodLabel(defaultLabel);
		classBuilder.getEmitter().emitGoto(continueBlock);
		//Else block end

        /* Emit the post-exception handler label, and pop the tag stack from
         * 'tagbodyStack'. */
		classBuilder.getEmitter().visitMethodLabel(continueBlock);

        /* TAGBODY always returns NIL, so put a NIL on the stack to be
         * returned. */
		classBuilder.getEmitter().emitGetstatic("lisp/common/type/Null", "NIL", "Llisp/common/type/Null;");

		//This is compilation only code
		classBuilder.getTagbodyStack().pop();
		classBuilder.getEmitter().visitTryCatchBlock(startTryBlock, catchBlock, catchBlock, "java/lang/Throwable");

		//Here is the finally code
		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "popTOCRecord", "()", "V", false);
		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "processReturnException", "()", "V", false);
	}

	private static Stack<TagbodyLabel> tagbodyReadLabels(ListStruct list, JavaClassBuilder classBuilder) {
		final Stack<TagbodyLabel> tagStack = new Stack<>();

		while (!list.equals(NullStruct.INSTANCE)) {
			final Object obj = list.getFirst();
			if (obj instanceof SymbolStruct) {
				// Insert the tag into the stack.
				int currentTagCounter = classBuilder.getTagCounter();
				currentTagCounter += 1;
				classBuilder.setTagCounter(currentTagCounter);
				tagStack.push(new TagbodyLabel((SymbolStruct) obj, new Label(), currentTagCounter));
			}
			list = list.getRest();
		}

		return tagStack;
	}

	private static TagbodyLabel findTagbodyBySymbol(final Stack<TagbodyLabel> stack, final SymbolStruct<?> symbol) {
		int size = stack.size();
		while (size-- > 0) {
			final TagbodyLabel tbl = stack.get(size);
			if (tbl.getSymbol().equals(symbol)) {
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
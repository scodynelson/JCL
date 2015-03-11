package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;

public class BlockCodeGenerator implements CodeGenerator<ListStruct> {

	public static final BlockCodeGenerator INSTANCE = new BlockCodeGenerator();

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {

		final Label startTryBlock = new Label();                //The start of the try block
		final Label catchBlock = new Label();                   //The start of the catch block
		final Label continueBlock = new Label();                //Subsequent code after the try/catch construct
		final Label ifBlock = new Label();                      //If block executed if this catches someone else's excepton

		// Get rid of the BLOCK symbol
		ListStruct restOfList = input.getRest();
		final SymbolStruct<?> sym = (SymbolStruct<?>) restOfList.getFirst();
		restOfList = restOfList.getRest();

		// ... ,
		codeGenerator.genCodeSpecialVariable(sym, classBuilder);
		// ..., sym

		classBuilder.getEmitter().emitGetstatic("lisp/system/TransferOfControl", "BLOCK", "Ljava/lang/String;");
		// ..., sym, BLOCK
		classBuilder.getEmitter().emitSwap();
		// ... , BLOCK, sym
		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "addTOCRecord", "(Ljava/lang/String;Ljava/lang/Object;)", "V", false);


        /* Call icgMainLoop() for each expression in the PROGN call,
		 * and remove all but the last expression's value from the stack  */
		classBuilder.getEmitter().visitMethodLabel(startTryBlock);
		while (!restOfList.equals(NullStruct.INSTANCE)) {
			codeGenerator.icgMainLoop(restOfList.getFirst(), classBuilder);
			restOfList = restOfList.getRest();
			if (!restOfList.equals(NullStruct.INSTANCE)) {
				classBuilder.getEmitter().emitNop();
				classBuilder.getEmitter().emitPop();
				classBuilder.getEmitter().emitNop();
			}
		}
		classBuilder.getEmitter().emitGoto(continueBlock);

		//Start catch block
		classBuilder.getEmitter().visitMethodLabel(catchBlock);
		// ..., throw_excep
		classBuilder.getEmitter().emitDup();
		// ..., throw_excep, throw_excep
		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "isMine", "(Ljava/lang/Throwable;)", "Ljava/lang/Object;", false);
		// ..., throw_excep, result
		classBuilder.getEmitter().emitDup();
		// ..., throw_excep, result, result
		classBuilder.getEmitter().emitIfnull(ifBlock);
		//Else block start
		// ..., throw_excep, result
		classBuilder.getEmitter().emitSwap();
		// ..., result, throw_excep
		classBuilder.getEmitter().emitPop();
		// ..., result
		classBuilder.getEmitter().emitGoto(continueBlock);
		//Else block end

		//If block start
		classBuilder.getEmitter().visitMethodLabel(ifBlock);
		// ..., throw_excep, result
		classBuilder.getEmitter().emitSwap();
		// ..., result, throw_excep
		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "setReturnException", "(Ljava/lang/Throwable;)", "V", false);
		// ..., result
		//If block end

		classBuilder.getEmitter().visitMethodLabel(continueBlock);

		classBuilder.getEmitter().visitTryCatchBlock(
				startTryBlock, //blockName + "_BlockA",
				catchBlock, //blockName + "_BlockB",
				catchBlock, //blockName + "_BlockB",
				"java/lang/Throwable");

		//Here is the finally code
		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "popTOCRecord", "()", "V", false);

		classBuilder.getEmitter().emitInvokestatic("lisp/system/TransferOfControl", "processReturnException", "()", "V", false);
	}
}

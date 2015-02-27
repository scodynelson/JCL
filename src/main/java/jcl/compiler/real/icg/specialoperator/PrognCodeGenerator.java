package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.system.EnhancedLinkedList;

public class PrognCodeGenerator implements CodeGenerator<ConsElement> {

	public static final PrognCodeGenerator INSTANCE = new PrognCodeGenerator();

	@Override
	public void generate(final ConsElement input, final IntermediateCodeGenerator codeGenerator) {
		// Get rid of the PROGN symbol
		EnhancedLinkedList<SimpleElement> restOfList = input.getElements().getAllButFirst();

        /* Call icgMainLoop() for each expression in the PROGN call,
		 * and remove all but the last expression's value from the stack  */
		while (!restOfList.equals(NullElement.INSTANCE)) {
			codeGenerator.icgMainLoop(restOfList.getFirst());
			restOfList = restOfList.getAllButFirst();
			if (!restOfList.equals(NullElement.INSTANCE)) {
				codeGenerator.emitter.emitPop();
			}
		}
	}
}

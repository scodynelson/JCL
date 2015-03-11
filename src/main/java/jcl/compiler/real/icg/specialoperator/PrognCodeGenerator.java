package jcl.compiler.real.icg.specialoperator;

import jcl.compiler.real.icg.CodeGenerator;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import org.springframework.stereotype.Component;

@Component
public class PrognCodeGenerator implements CodeGenerator<ListStruct> {

	@Override
	public void generate(final ListStruct input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
		// Get rid of the PROGN symbol
		ListStruct restOfList = input.getRest();

        /* Call icgMainLoop() for each expression in the PROGN call,
		 * and remove all but the last expression's value from the stack  */
		while (!restOfList.equals(NullStruct.INSTANCE)) {
			codeGenerator.icgMainLoop(restOfList.getFirst(), classBuilder);
			restOfList = restOfList.getRest();
			if (!restOfList.equals(NullStruct.INSTANCE)) {
				classBuilder.getEmitter().emitPop();
			}
		}
	}
}

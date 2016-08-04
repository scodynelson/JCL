package jcl.compiler.icg.generator;

import java.util.List;
import java.util.ListIterator;
import java.util.stream.Collectors;

import jcl.compiler.icg.CodeGenerator;
import jcl.compiler.icg.GeneratorEvent;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.struct.specialoperator.QuoteStruct;
import jcl.lang.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.SymbolStructImpl;
import jcl.lang.condition.exception.ProgramErrorException;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * Class to perform 'quote' special operator code generation.
 */
@Component
final class QuoteCodeGenerator implements CodeGenerator<QuoteStruct> {

	/**
	 * {@link IntermediateCodeGenerator} used for generating the {@link QuoteStruct#object} value.
	 */
	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	/**
	 * {@inheritDoc}
	 * Generation method for {@link QuoteStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link QuoteStruct#object} value</li>
	 * </ol>
	 *
	 * @param input
	 * 		the {@link QuoteStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@EventListener
	public void onGeneratorEvent(final GeneratorEvent<QuoteStruct> event) {
		final QuoteStruct input = event.getSource();
		final GeneratorState generatorState = event.getGeneratorState();
		final LispStruct quotedObject = input.getObject();
		generateQuotedObject(quotedObject, generatorState);
	}

	/**
	 * Generation method for quoted {@link LispStruct} objects. It performs the following operations:
	 * <ol>
	 * <li>Generating the value via {@link #generateQuotedSymbol(SymbolStructImpl, GeneratorState)} if the {@code
	 * quotedObject} is a {@link SymbolStructImpl}</li>
	 * <li>Generating the value via {@link #generateQuotedCons(ConsStruct, GeneratorState)} if the {@code quotedObject}
	 * is a {@link ConsStruct}</li>
	 * <li>Generating the value via {@link IntermediateCodeGenerator#generate(LispStruct, GeneratorState)} if the
	 * {@code
	 * quotedObject} is neither a {@link SymbolStructImpl} nor a {@link ConsStruct}</li>
	 * </ol>
	 *
	 * @param quotedObject
	 * 		the 'quoted' {@link LispStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	@SuppressWarnings("ChainOfInstanceofChecks")
	private void generateQuotedObject(final LispStruct quotedObject, final GeneratorState generatorState) {
		if (quotedObject instanceof SymbolStructImpl) {
			generateQuotedSymbol((SymbolStructImpl) quotedObject, generatorState);
		} else if (quotedObject instanceof ConsStruct) {
			generateQuotedCons((ConsStruct) quotedObject, generatorState);
		} else {
			codeGenerator.generate(quotedObject, generatorState);
		}
	}

	/**
	 * Generation method for quoted {@link SymbolStructImpl} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link SymbolStructImpl} value</li>
	 * <li>Ensuring the generated {@link SymbolStructImpl} value is loaded on the top of the stack</li>
	 * </ol>
	 * As an example, it will transform {@code 'x} into the following Java code:
	 * <pre>
	 * {@code
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.findSymbol("X").getSymbol();
	 * }
	 * </pre>
	 *
	 * @param quotedSymbol
	 * 		the 'quoted' {@link SymbolStructImpl} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	private static void generateQuotedSymbol(final SymbolStructImpl quotedSymbol, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int packageStore = methodBuilder.getNextAvailableStore();
		final int symbolStore = methodBuilder.getNextAvailableStore();
		CodeGenerators.generateSymbol(quotedSymbol, generatorState, packageStore, symbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
	}

	/**
	 * Generation method for quoted {@link ConsStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Checking whether or not {@link ConsStruct#isCircular()} is true, throwing a {@link ProgramErrorException} if
	 * so</li>
	 * <li>Looping throw the {@link ConsStruct} in reverse order using a {@link ListIterator}, generating each element
	 * into its appropriate embedded {@link ConsStruct}</li>
	 * <li>Creating a dotted {@link ConsStruct} for the final 2 elements first if {@link ConsStruct#isDotted()} is
	 * true</li>
	 * </ol>
	 * As an example, it will transform {@code '(x)} into the following Java code:
	 * <pre>
	 * {@code
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP-USER");
	 *      SymbolStruct var3 = var2.findSymbol("X").getSymbol();
	 *      ConsStruct var5 = new ConsStruct(var3);
	 * }
	 * </pre>
	 *
	 * @param quotedCons
	 * 		the 'quoted' {@link ConsStruct} input value to generate code for
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 */
	private void generateQuotedCons(final ConsStruct quotedCons, final GeneratorState generatorState) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		if (quotedCons.isCircular()) {
			throw new ProgramErrorException("Generation of circular lists is not yet supported.");
		}

		final List<LispStruct> lispStructs = quotedCons.stream().collect(Collectors.toList());
		final ListIterator<LispStruct> listIterator = lispStructs.listIterator(lispStructs.size());

		LispStruct previousCdr = listIterator.previous();
		generateQuotedObject(previousCdr, generatorState);

		final int lastElementStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, lastElementStore);

		if (quotedCons.isDotted()) {
			previousCdr = listIterator.previous();
			generateQuotedObject(previousCdr, generatorState);

			final int secondToLastElementStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, secondToLastElementStore);

			mv.visitVarInsn(Opcodes.ALOAD, secondToLastElementStore);
			mv.visitVarInsn(Opcodes.ALOAD, lastElementStore);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.LISP_STRUCT_FACTORY_NAME,
			                   GenerationConstants.LISP_STRUCT_FACTORY_TO_CONS_CAR_CDR_METHOD_NAME,
			                   GenerationConstants.LISP_STRUCT_FACTORY_TO_CONS_CAR_CDR_METHOD_DESC,
			                   false);
		} else {
			mv.visitVarInsn(Opcodes.ALOAD, lastElementStore);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.LISP_STRUCT_FACTORY_NAME,
			                   GenerationConstants.LISP_STRUCT_FACTORY_TO_CONS_CAR_METHOD_NAME,
			                   GenerationConstants.LISP_STRUCT_FACTORY_TO_CONS_CAR_METHOD_DESC,
			                   false);
		}
		final int previousConsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, previousConsStore);

		final int nextElementStore = methodBuilder.getNextAvailableStore();

		while (listIterator.hasPrevious()) {
			previousCdr = listIterator.previous();
			generateQuotedObject(previousCdr, generatorState);

			mv.visitVarInsn(Opcodes.ASTORE, nextElementStore);

			mv.visitVarInsn(Opcodes.ALOAD, nextElementStore);
			mv.visitVarInsn(Opcodes.ALOAD, previousConsStore);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.LISP_STRUCT_FACTORY_NAME,
			                   GenerationConstants.LISP_STRUCT_FACTORY_TO_CONS_CAR_CDR_METHOD_NAME,
			                   GenerationConstants.LISP_STRUCT_FACTORY_TO_CONS_CAR_CDR_METHOD_DESC,
			                   false);
			mv.visitVarInsn(Opcodes.ASTORE, previousConsStore);
		}

		mv.visitVarInsn(Opcodes.ALOAD, previousConsStore);
	}
}

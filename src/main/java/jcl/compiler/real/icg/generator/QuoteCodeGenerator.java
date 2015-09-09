package jcl.compiler.real.icg.generator;

import java.util.List;
import java.util.ListIterator;

import jcl.LispStruct;
import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.QuoteStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ConsStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class QuoteCodeGenerator implements CodeGenerator<QuoteStruct> {

	@Autowired
	private IntermediateCodeGenerator codeGenerator;

	@Override
	public void generate(final QuoteStruct input, final GeneratorState generatorState) {

		final LispStruct quotedObject = input.getObject();
		generateQuotedObject(quotedObject, generatorState);
	}

	public void generateQuotedObject(final LispStruct quotedObject, final GeneratorState classBuilder) {
		if (quotedObject instanceof SymbolStruct) {
			generateQuotedSymbol((SymbolStruct) quotedObject, classBuilder);
		} else if (quotedObject instanceof ConsStruct) {
			generateQuotedCons((ConsStruct) quotedObject, classBuilder);
		} else {
			codeGenerator.generate(quotedObject, classBuilder);
		}
	}

	private static void generateQuotedSymbol(final SymbolStruct<?> quotedSymbol, final GeneratorState classBuilder) {

		final JavaMethodBuilder methodBuilder = classBuilder.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final int packageStore = methodBuilder.getNextAvailableStore();
		final int symbolStore = methodBuilder.getNextAvailableStore();
		SymbolCodeGeneratorUtil.generate(quotedSymbol, classBuilder, packageStore, symbolStore);

		mv.visitVarInsn(Opcodes.ALOAD, symbolStore);
	}

	private void generateQuotedCons(final ConsStruct quotedCons, final GeneratorState classBuilder) {

		final JavaMethodBuilder methodBuilder = classBuilder.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		if (quotedCons.isCircular()) {
			throw new ProgramErrorException("Generation of circular lists is not yet supported.");
		}

		final List<LispStruct> lispStructs = quotedCons.getAsJavaList();
		final ListIterator<LispStruct> listIterator = lispStructs.listIterator(lispStructs.size());

		LispStruct previousCdr = listIterator.previous();
		generateQuotedObject(previousCdr, classBuilder);
		final int lastElementStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, lastElementStore);

		if (quotedCons.isDotted()) {
			previousCdr = listIterator.previous();
			generateQuotedObject(previousCdr, classBuilder);
			final int secondToLastElementStore = methodBuilder.getNextAvailableStore();
			mv.visitVarInsn(Opcodes.ASTORE, secondToLastElementStore);

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.CONS_STRUCT_NAME);
			mv.visitInsn(Opcodes.DUP);

			mv.visitVarInsn(Opcodes.ALOAD, secondToLastElementStore);
			mv.visitVarInsn(Opcodes.ALOAD, lastElementStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					GenerationConstants.CONS_STRUCT_NAME,
					GenerationConstants.INIT_METHOD_NAME,
					GenerationConstants.CONS_STRUCT_INIT_CAR_CDR_DESC,
					false);
		} else {
			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.CONS_STRUCT_NAME);
			mv.visitInsn(Opcodes.DUP);

			mv.visitVarInsn(Opcodes.ALOAD, lastElementStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					GenerationConstants.CONS_STRUCT_NAME,
					GenerationConstants.INIT_METHOD_NAME,
					GenerationConstants.CONS_STRUCT_INIT_CAR_DESC,
					false);
		}

		final int previousConsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, previousConsStore);

		final int nextElementStore = methodBuilder.getNextAvailableStore();

		while (listIterator.hasPrevious()) {
			previousCdr = listIterator.previous();
			generateQuotedObject(previousCdr, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, nextElementStore);

			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.CONS_STRUCT_NAME);
			mv.visitInsn(Opcodes.DUP);

			mv.visitVarInsn(Opcodes.ALOAD, nextElementStore);
			mv.visitVarInsn(Opcodes.ALOAD, previousConsStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					GenerationConstants.CONS_STRUCT_NAME,
					GenerationConstants.INIT_METHOD_NAME,
					GenerationConstants.CONS_STRUCT_INIT_CAR_CDR_DESC,
					false);
			mv.visitVarInsn(Opcodes.ASTORE, previousConsStore);
		}

		mv.visitVarInsn(Opcodes.ALOAD, previousConsStore);
	}
}

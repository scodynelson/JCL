package jcl.compiler.real.icg.generator.specialoperator;

import java.util.List;
import java.util.ListIterator;

import jcl.LispStruct;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.icg.generator.FormGenerator;
import jcl.compiler.real.icg.generator.GenerationConstants;
import jcl.compiler.real.struct.specialoperator.QuoteStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.lists.ConsStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class QuoteCodeGenerator implements CodeGenerator<QuoteStruct> {

	@Autowired
	private FormGenerator formGenerator;

	@Override
	public void generate(final QuoteStruct input, final JavaClassBuilder classBuilder) {

		final LispStruct quotedObject = input.getObject();
		generateQuotedObject(quotedObject, classBuilder);
	}

	public void generateQuotedObject(final LispStruct quotedObject, final JavaClassBuilder classBuilder) {
		if (quotedObject instanceof SymbolStruct) {
			generateQuotedSymbol((SymbolStruct) quotedObject, classBuilder);
		} else if (quotedObject instanceof ConsStruct) {
			generateQuotedCons((ConsStruct) quotedObject, classBuilder);
		} else {
			formGenerator.generate(quotedObject, classBuilder);
		}
	}

	private void generateQuotedSymbol(final SymbolStruct<?> quotedSymbol, final JavaClassBuilder classBuilder) {

		final JavaMethodBuilder methodBuilder = classBuilder.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final String packageName = quotedSymbol.getSymbolPackage().getName();
		final String symbolName = quotedSymbol.getName();

		mv.visitLdcInsn(packageName);
		mv.visitMethodInsn(Opcodes.INVOKESTATIC,
				GenerationConstants.PACKAGE_STRUCT_NAME,
				GenerationConstants.PACKAGE_STRUCT_FIND_PACKAGE_METHOD_NAME,
				GenerationConstants.PACKAGE_STRUCT_FIND_PACKAGE_METHOD_DESC,
				false);
		final int packageStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, packageStore);

		mv.visitVarInsn(Opcodes.ALOAD, packageStore);
		mv.visitLdcInsn(symbolName);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.PACKAGE_STRUCT_NAME,
				GenerationConstants.PACKAGE_STRUCT_FIND_SYMBOL_METHOD_NAME,
				GenerationConstants.PACKAGE_STRUCT_FIND_SYMBOL_METHOD_DESC,
				false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.PACKAGE_SYMBOL_STRUCT_NAME,
				GenerationConstants.PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_NAME,
				GenerationConstants.PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_DESC,
				false);
	}

	private void generateQuotedCons(final ConsStruct quotedCons, final JavaClassBuilder classBuilder) {

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

			mv.visitTypeInsn(Opcodes.NEW, "jcl/lists/ConsStruct");
			mv.visitInsn(Opcodes.DUP);

			mv.visitVarInsn(Opcodes.ALOAD, secondToLastElementStore);
			mv.visitVarInsn(Opcodes.ALOAD, lastElementStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/lists/ConsStruct", "<init>", "(Ljcl/LispStruct;Ljcl/LispStruct;)V", false);
		} else {
			mv.visitTypeInsn(Opcodes.NEW, "jcl/lists/ConsStruct");
			mv.visitInsn(Opcodes.DUP);

			mv.visitVarInsn(Opcodes.ALOAD, lastElementStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/lists/ConsStruct", "<init>", "(Ljcl/LispStruct;)V", false);
		}

		final int previousConsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, previousConsStore);

		final int nextElementStore = methodBuilder.getNextAvailableStore();

		while (listIterator.hasPrevious()) {
			previousCdr = listIterator.previous();
			generateQuotedObject(previousCdr, classBuilder);
			mv.visitVarInsn(Opcodes.ASTORE, nextElementStore);

			mv.visitTypeInsn(Opcodes.NEW, "jcl/lists/ConsStruct");
			mv.visitInsn(Opcodes.DUP);

			mv.visitVarInsn(Opcodes.ALOAD, nextElementStore);
			mv.visitVarInsn(Opcodes.ALOAD, previousConsStore);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/lists/ConsStruct", "<init>", "(Ljcl/LispStruct;Ljcl/LispStruct;)V", false);
			mv.visitVarInsn(Opcodes.ASTORE, previousConsStore);
		}

		mv.visitVarInsn(Opcodes.ALOAD, previousConsStore);
	}
}

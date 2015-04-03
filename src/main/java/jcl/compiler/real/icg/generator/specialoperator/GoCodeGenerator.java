package jcl.compiler.real.icg.generator.specialoperator;

import java.util.ListIterator;
import java.util.Set;
import java.util.Stack;

import jcl.LispStruct;
import jcl.compiler.real.icg.ClassDef;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.generator.CodeGenerator;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.printer.Printer;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class GoCodeGenerator implements CodeGenerator<GoStruct<?>> {

	@Autowired
	private Printer printer;

	@Override
	public void generate(final GoStruct<?> input, final JavaClassBuilder classBuilder) {

		final ClassDef currentClass = classBuilder.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();
		final MethodVisitor previousMv = currentClass.getMethodVisitor();

		final String goMethodName = "go_" + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, goMethodName, "()Ljcl/LispStruct;", null, null);
		currentClass.setMethodVisitor(mv);
		mv.visitCode();

		final Stack<Set<TagbodyLabel>> tagbodyLabelStack = classBuilder.getTagbodyLabelStack();
		final TagbodyLabel tagbodyLabel = getTagbodyLabel(tagbodyLabelStack, input);

		final int index = tagbodyLabel.getIndex();
		mv.visitLdcInsn(index);
		final int tagIndexStore = currentClass.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, tagIndexStore);

		mv.visitTypeInsn(Opcodes.NEW, "jcl/compiler/real/icg/generator/specialoperator/exception/GoException");
		mv.visitInsn(Opcodes.DUP);
		mv.visitVarInsn(Opcodes.ILOAD, tagIndexStore);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL, "jcl/compiler/real/icg/generator/specialoperator/exception/GoException", "<init>", "(I)V", false);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		currentClass.setMethodVisitor(previousMv);

		previousMv.visitVarInsn(Opcodes.ALOAD, 0);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, goMethodName, "()Ljcl/LispStruct;", false);
	}

	private TagbodyLabel getTagbodyLabel(final Stack<Set<TagbodyLabel>> tagbodyLabelStack, final GoStruct<?> tagToFind) {

		final ListIterator<Set<TagbodyLabel>> tagbodyLabelListIterator = tagbodyLabelStack.listIterator(tagbodyLabelStack.size());

		TagbodyLabel tagbodyLabel = null;

		out:
		while (tagbodyLabelListIterator.hasPrevious()) {
			final Set<TagbodyLabel> previousStack = tagbodyLabelListIterator.previous();
			for (final TagbodyLabel currentTBL : previousStack) {
				final GoStruct<?> goTag = currentTBL.getTag();
				if (tagToFind.equals(goTag)) {
					tagbodyLabel = currentTBL;
					break out;
				}
			}
		}

		if (tagbodyLabel == null) {
			final LispStruct tagObject = tagToFind.getTag();
			final String printedTagObject = printer.print(tagObject);
			throw new ProgramErrorException("GO: No TAGBODY with Tag " + printedTagObject + " is visible.");
		}

		return tagbodyLabel;
	}
}

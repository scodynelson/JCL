package jcl.compiler.real.icg.generator;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;

import jcl.compiler.real.icg.GeneratorState;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.compiler.real.struct.specialoperator.PrognStruct;
import jcl.compiler.real.struct.specialoperator.TagbodyStruct;
import jcl.compiler.real.struct.specialoperator.go.GoStruct;
import jcl.lists.NullStruct;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
class TagbodyCodeGenerator extends SpecialOperatorCodeGenerator<TagbodyStruct> {

	@Autowired
	private PrognCodeGenerator prognCodeGenerator;

	@Autowired
	private NullCodeGenerator nullCodeGenerator;

	private TagbodyCodeGenerator() {
		super("tagbody_");
	}

	@Override
	public void generate(final TagbodyStruct input, final GeneratorState generatorState) {

		final Map<GoStruct<?>, PrognStruct> tagbodyForms = input.getTagbodyForms();

		final JavaClassBuilder currentClass = generatorState.getCurrentClass();
		final String fileName = currentClass.getFileName();

		final ClassWriter cw = currentClass.getClassWriter();

		final String tagbodyMethodName = methodNamePrefix + System.nanoTime();
		final MethodVisitor mv = cw.visitMethod(Opcodes.ACC_PRIVATE, tagbodyMethodName, SPECIAL_OPERATOR_METHOD_DESC, null, null);

		final JavaMethodBuilder methodBuilder = new JavaMethodBuilder(mv);
		final Stack<JavaMethodBuilder> methodBuilderStack = generatorState.getMethodBuilderStack();
		methodBuilderStack.push(methodBuilder);

		mv.visitCode();
		final int thisStore = methodBuilder.getNextAvailableStore();
		final int closureArgStore = methodBuilder.getNextAvailableStore();

		final Label tryBlockStart = new Label();
		final Label tryBlockEnd = new Label();
		final Label catchBlockStart = new Label();
		final Label catchBlockEnd = new Label();
		mv.visitTryCatchBlock(tryBlockStart, tryBlockEnd, catchBlockStart, GenerationConstants.GO_EXCEPTION_NAME);

		final Map<TagbodyLabel, PrognStruct> tagbodyLabeledForms = getTagbodyLabeledForms(tagbodyForms, generatorState);

		final Set<TagbodyLabel> tagbodyLabels = tagbodyLabeledForms.keySet();
		generatorState.getTagbodyLabelStack().push(tagbodyLabels);

		mv.visitLabel(tryBlockStart);

		for (final Map.Entry<TagbodyLabel, PrognStruct> tagbodyLabeledForm : tagbodyLabeledForms.entrySet()) {
			final TagbodyLabel tagbodyLabel = tagbodyLabeledForm.getKey();
			final PrognStruct forms = tagbodyLabeledForm.getValue();

			final Label tagLabel = tagbodyLabel.getLabel();
			mv.visitLabel(tagLabel);

			prognCodeGenerator.generate(forms, generatorState);
			mv.visitInsn(Opcodes.POP);
		}

		mv.visitLabel(tryBlockEnd);
		mv.visitJumpInsn(Opcodes.GOTO, catchBlockEnd);

		mv.visitLabel(catchBlockStart);
		final int goExceptionStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, goExceptionStore);

		mv.visitVarInsn(Opcodes.ALOAD, goExceptionStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
				GenerationConstants.GO_EXCEPTION_NAME,
				GenerationConstants.GO_EXCEPTION_NAME,
				GenerationConstants.GO_EXCEPTION_GET_TAG_INDEX_METHOD_DESC,
				false);
		final int goExceptionIndexStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ISTORE, goExceptionIndexStore);

		// NOTE: the 'tagbodyLabelList' here will be properly ordered because the 'tagbodyLabels' are an ordered LinkedKeySet
		//       thanks to the usage of 'LinkedHashMap'
		final List<TagbodyLabel> tagbodyLabelList = new ArrayList<>(tagbodyLabels);

		final int tagsSize = tagbodyLabelList.size();
		final int[] tagNumbers = new int[tagsSize];
		final Label[] tagLabels = new Label[tagsSize];

		for (int index = 0; index < tagsSize; index++) {
			final TagbodyLabel indexedLabel = tagbodyLabelList.get(index);

			tagNumbers[index] = indexedLabel.getIndex();
			tagLabels[index] = indexedLabel.getLabel();
		}

		mv.visitVarInsn(Opcodes.ILOAD, goExceptionIndexStore);

		final int minKey = tagNumbers[0];
		final int maxKey = tagNumbers[tagsSize - 1];
		final Label defaultCase = new Label();
		mv.visitTableSwitchInsn(minKey, maxKey, defaultCase, tagLabels);

		mv.visitLabel(defaultCase);
		mv.visitVarInsn(Opcodes.ALOAD, goExceptionStore);
		mv.visitInsn(Opcodes.ATHROW);

		mv.visitLabel(catchBlockEnd);
		nullCodeGenerator.generate(NullStruct.INSTANCE, generatorState);

		mv.visitInsn(Opcodes.ARETURN);

		mv.visitMaxs(-1, -1);
		mv.visitEnd();

		methodBuilderStack.pop();

		final JavaMethodBuilder previousMethodBuilder = methodBuilderStack.peek();
		final MethodVisitor previousMv = previousMethodBuilder.getMethodVisitor();

		previousMv.visitVarInsn(Opcodes.ALOAD, thisStore);
		previousMv.visitVarInsn(Opcodes.ALOAD, closureArgStore);
		previousMv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, fileName, tagbodyMethodName, SPECIAL_OPERATOR_METHOD_DESC, false);
	}

	private static Map<TagbodyLabel, PrognStruct> getTagbodyLabeledForms(final Map<GoStruct<?>, PrognStruct> tagbodyForms,
	                                                                     final GeneratorState classBuilder) {

		// NOTE: use LinkedHashMap so the tags and forms are ordered appropriately
		final Map<TagbodyLabel, PrognStruct> tagbodyLabeledForms = new LinkedHashMap<>();
		for (final Map.Entry<GoStruct<?>, PrognStruct> tagbodyForm : tagbodyForms.entrySet()) {
			final GoStruct<?> tag = tagbodyForm.getKey();
			final PrognStruct forms = tagbodyForm.getValue();

			final int nextTagbodyTagIndex = classBuilder.getNextTagbodyTagIndex();
			final TagbodyLabel tagbodyLabel = new TagbodyLabel(tag, nextTagbodyTagIndex, new Label());
			tagbodyLabeledForms.put(tagbodyLabel, forms);
		}

		return tagbodyLabeledForms;
	}
}

/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import java.util.List;

import jcl.compiler.function.Closure;
import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.compiler.icg.generator.GenerationConstants;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;

public class MultipleValueCallStruct extends CompilerSpecialOperatorStruct {

	private final CompilerFunctionStruct functionForm;

	private final List<LispStruct> forms;

	public MultipleValueCallStruct(final CompilerFunctionStruct functionForm, final List<LispStruct> forms) {
		super("multipleValueCall");
		this.functionForm = functionForm;
		this.forms = forms;
	}

	public CompilerFunctionStruct getFunctionForm() {
		return functionForm;
	}

	public List<LispStruct> getForms() {
		return forms;
	}

	/**
	 * Constant {@link String} containing the error message prefix for the customized {@link ProgramErrorException}
	 * thrown if the {@link MultipleValueCallStruct#functionForm} does not produce a {@link FunctionStruct} value.
	 */
	private static final String NOT_FUNCTION_ERROR_STRING = "MULTIPLE-VALUE-CALL: Invalid function form: ";

	/**
	 * {@inheritDoc}
	 * Generation method for {@link MultipleValueCallStruct} objects, by performing the following operations:
	 * <ol>
	 * <li>Generating the {@link MultipleValueCallStruct#functionForm} value</li>
	 * <li>Verifying if the generated function value is an instance of type {@link FunctionStruct}, throwing a
	 * customized {@link ProgramErrorException} if it is not</li>
	 * <li>After verifying the function value is a {@link FunctionStruct}, the parameter application list is built</li>
	 * <li>Each of the {@link MultipleValueCallStruct#forms} are generated and added to the new parameter {@link
	 * List}</li>
	 * <li>After the parameter {@link List} is built, it is then converted to an array of {@link LispStruct}s to be
	 * used as parameters via {@link List#toArray(Object[])}</li>
	 * <li>Finally, the {@link FunctionStruct} is invoked passing the new array of {@link LispStruct}s as its
	 * arguments</li>
	 * </ol>
	 * As an example, it will transform {@code (multiple-value-call #'+ 1 2)} into the following Java code:
	 * <pre>
	 * {@code
	 * private LispStruct multipleValueCall_1(Closure var1) {
	 *      PackageStruct var2 = PackageStruct.findPackage("COMMON-LISP");
	 *      SymbolStruct var3 = var2.findSymbol("+").getSymbol();
	 *
	 *      LispStruct var4 = var3.getFunction();
	 *      if(!(var4 instanceof FunctionStructImpl)) {
	 *          throw new ProgramErrorException("MULTIPLE-VALUE-CALL: Invalid function form: " + var4);
	 *      } else {
	 *          FunctionStructImpl var5 = (FunctionStructImpl)var4;
	 *          List var6 = new ArrayList();
	 *
	 *          BigInteger var8 = new BigInteger("1");
	 *          IntegerStruct var7 = new IntIntegerStruct(var8);
	 *          ValuesStruct.addValuesToList(var6, var7);
	 *
	 *          BigInteger var9 = new BigInteger("2");
	 *          var7 = new IntIntegerStruct(var9);
	 *          ValuesStruct.addValuesToList(var6, var7);
	 *
	 *          LispStruct[] var10 = new LispStruct[var6.size()];
	 *          var10 = (LispStruct[])var6.toArray(var10);
	 *          return var5.apply(var10);
	 *      }
	 * }
	 * }
	 * </pre>
	 *
	 * @param generatorState
	 * 		stateful object used to hold the current state of the code generation process
	 * @param methodBuilder
	 * 		{@link JavaMethodBuilder} used for building a Java method body
	 * @param closureArgStore
	 * 		the storage location index on the stack where the {@link Closure} argument exists
	 */
	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaMethodBuilder methodBuilder,
	                                       final int closureArgStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		functionForm.generate(generatorState);

		final int functionFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, functionFormStore);

		final Label functionCheckIfEnd = new Label();

		mv.visitVarInsn(Opcodes.ALOAD, functionFormStore);
		mv.visitTypeInsn(Opcodes.INSTANCEOF, GenerationConstants.FUNCTION_STRUCT_NAME);
		mv.visitJumpInsn(Opcodes.IFNE, functionCheckIfEnd);

		generateProgramError(mv, functionFormStore);

		mv.visitLabel(functionCheckIfEnd);

		mv.visitVarInsn(Opcodes.ALOAD, functionFormStore);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.FUNCTION_STRUCT_NAME);
		final int realFunctionFormStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, realFunctionFormStore);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_ARRAY_LIST_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.JAVA_ARRAY_LIST_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.JAVA_ARRAY_LIST_INIT_DESC,
		                   false);
		final int argsListStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, argsListStore);

		final int formStore = methodBuilder.getNextAvailableStore();

		for (final LispStruct form : forms) {
			form.generate(generatorState);
			mv.visitVarInsn(Opcodes.ASTORE, formStore);

			mv.visitVarInsn(Opcodes.ALOAD, argsListStore);
			mv.visitVarInsn(Opcodes.ALOAD, formStore);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.VALUES_STRUCT_NAME,
			                   GenerationConstants.VALUES_STRUCT_ADD_VALUES_TO_LIST_METHOD_NAME,
			                   GenerationConstants.VALUES_STRUCT_ADD_VALUES_TO_LIST_METHOD_DESC,
			                   false);
		}

		mv.visitVarInsn(Opcodes.ALOAD, argsListStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
		                   GenerationConstants.JAVA_LIST_NAME,
		                   GenerationConstants.JAVA_LIST_SIZE_METHOD_NAME,
		                   GenerationConstants.JAVA_LIST_SIZE_METHOD_DESC,
		                   true);
		mv.visitTypeInsn(Opcodes.ANEWARRAY, GenerationConstants.LISP_STRUCT_NAME);
		final int argsStore = methodBuilder.getNextAvailableStore();
		mv.visitVarInsn(Opcodes.ASTORE, argsStore);

		mv.visitVarInsn(Opcodes.ALOAD, argsListStore);
		mv.visitVarInsn(Opcodes.ALOAD, argsStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
		                   GenerationConstants.JAVA_LIST_NAME,
		                   GenerationConstants.JAVA_LIST_TO_ARRAY_METHOD_NAME,
		                   GenerationConstants.JAVA_LIST_TO_ARRAY_METHOD_DESC,
		                   true);
		mv.visitTypeInsn(Opcodes.CHECKCAST, GenerationConstants.LISP_STRUCT_ARRAY_DESC);
		mv.visitVarInsn(Opcodes.ASTORE, argsStore);

		mv.visitVarInsn(Opcodes.ALOAD, realFunctionFormStore);
		mv.visitVarInsn(Opcodes.ALOAD, argsStore);
		mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
		                   GenerationConstants.FUNCTION_STRUCT_NAME,
		                   GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_NAME,
		                   GenerationConstants.FUNCTION_STRUCT_APPLY_METHOD_DESC,
		                   true);

		mv.visitInsn(Opcodes.ARETURN);
	}

	/**
	 * Private method for generating the code to create and throw a customized {@link ProgramErrorException} when the
	 * {@link MultipleValueCallStruct#functionForm} does not generate a {@link FunctionStruct} value.
	 *
	 * @param mv
	 * 		the current {@link MethodVisitor} to generate the code inside
	 * @param functionFormStore
	 * 		the storage location index on the stack where the function form generated {@link LispStruct} value exists
	 */
	private static void generateProgramError(final MethodVisitor mv, final int functionFormStore) {
		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.PROGRAM_ERROR_EXCEPTION_NAME);
		mv.visitInsn(Opcodes.DUP);

		mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.JAVA_STRING_BUILDER_NAME);
		mv.visitInsn(Opcodes.DUP);
		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.JAVA_STRING_BUILDER_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.JAVA_STRING_BUILDER_INIT_DESC,
		                   false);
		mv.visitLdcInsn(NOT_FUNCTION_ERROR_STRING);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.JAVA_STRING_BUILDER_NAME,
		                   GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_NAME,
		                   GenerationConstants.JAVA_STRING_BUILDER_APPEND_STRING_METHOD_DESC,
		                   false);
		mv.visitVarInsn(Opcodes.ALOAD, functionFormStore);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.JAVA_STRING_BUILDER_NAME,
		                   GenerationConstants.JAVA_STRING_BUILDER_APPEND_METHOD_NAME,
		                   GenerationConstants.JAVA_STRING_BUILDER_APPEND_OBJECT_METHOD_DESC,
		                   false);
		mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
		                   GenerationConstants.JAVA_STRING_BUILDER_NAME,
		                   GenerationConstants.JAVA_STRING_BUILDER_TO_STRING_METHOD_NAME,
		                   GenerationConstants.JAVA_STRING_BUILDER_TO_STRING_METHOD_DESC,
		                   false);

		mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
		                   GenerationConstants.PROGRAM_ERROR_EXCEPTION_NAME,
		                   GenerationConstants.INIT_METHOD_NAME,
		                   GenerationConstants.PROGRAM_ERROR_EXCEPTION_INIT_STRING_DESC,
		                   false);
		mv.visitInsn(Opcodes.ATHROW);
	}
}

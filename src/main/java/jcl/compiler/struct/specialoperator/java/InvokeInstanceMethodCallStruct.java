package jcl.compiler.struct.specialoperator.java;

import java.lang.reflect.Method;
import java.util.List;
import java.util.stream.Collectors;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaEnvironmentMethodBuilder;
import jcl.compiler.icg.generator.CodeGenerators;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;
import jcl.lang.java.JavaMethodStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

import static org.objectweb.asm.Opcodes.INVOKEINTERFACE;
import static org.objectweb.asm.Opcodes.INVOKEVIRTUAL;

public class InvokeInstanceMethodCallStruct extends CompilerSpecialOperatorStruct {

	private final JavaMethodStruct methodStruct;
	private final LispStruct object;
	protected final List<LispStruct> arguments;

	public InvokeInstanceMethodCallStruct(final JavaMethodStruct methodStruct, final LispStruct object,
	                                      final List<LispStruct> arguments) {
		super("javaInstanceMethodCall");
		this.object = object;
		this.methodStruct = methodStruct;
		this.arguments = arguments;
	}

	@Override
	protected void generateSpecialOperator(final GeneratorState generatorState, final JavaEnvironmentMethodBuilder methodBuilder) {
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final Method method = methodStruct.getJavaMethod();
		final Class<?> declaringClass = method.getDeclaringClass();
		final String invocationMethod = method.getName();

		// Generate the Object to invoke the method
		object.generate(generatorState);
		mv.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(declaringClass));

		final Class<?>[] parameterTypes = method.getParameterTypes();

		// Generate the Method Arguments
		for (int i = 0; i < arguments.size(); i++) {
			final LispStruct argument = arguments.get(i);
			argument.generate(generatorState);
			mv.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(parameterTypes[i]));
		}

		final int opcode = declaringClass.isInterface() ? INVOKEINTERFACE : INVOKEVIRTUAL;

		mv.visitMethodInsn(opcode,
		                   Type.getInternalName(declaringClass),
		                   invocationMethod,
		                   CodeGenerators.getMethodDescription(declaringClass, invocationMethod, parameterTypes),
		                   declaringClass.isInterface());
		mv.visitInsn(Opcodes.ARETURN);
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder();

		final Method method = methodStruct.getJavaMethod();

		builder.append(object);
		builder.append('.');
		builder.append(method.getName());
		builder.append('(');

		final String argumentsString
				= arguments.stream()
				           .map(Object::toString)
				           .collect(Collectors.joining(", "));
		builder.append(argumentsString);
		builder.append(')');

		return builder.toString();
	}
}
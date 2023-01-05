/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.icg.generator;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import jcl.compiler.icg.GeneratorState;
import jcl.compiler.icg.JavaClassBuilder;
import jcl.compiler.icg.JavaMethodBuilder;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import lombok.experimental.UtilityClass;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

@UtilityClass
public final class CodeGenerators {

	public static <TYPE> String getConstructorDescription(final Class<TYPE> clazz, final Class<?>... parameterTypes) {
		final Constructor<TYPE> constructor = getConstructor(clazz, parameterTypes);
		if (constructor == null) {
			return null;
		} else {
			return Type.getConstructorDescriptor(constructor);
		}
	}

	private static <TYPE> Constructor<TYPE> getConstructor(final Class<TYPE> clazz, final Class<?>... parameterTypes) {
		try {
			return clazz.getDeclaredConstructor(parameterTypes);
		} catch (final NoSuchMethodException ignored) {
			return null;
		}
	}

	public static <TYPE> String getMethodDescription(final Class<TYPE> clazz, final String methodName, final Class<?>... parameterTypes) {
		final Method method = getMethod(clazz, methodName, parameterTypes);
		if (method == null) {
			return null;
		} else {
			return Type.getMethodDescriptor(method);
		}
	}

	private static Method getMethod(final Class<?> clazz, final String methodName, final Class<?>... parameterTypes) {
		try {
			return clazz.getMethod(methodName, parameterTypes);
		} catch (final NoSuchMethodException ignored) {
			return null;
		}
	}

	public static void generateSymbol(final SymbolStruct input, final GeneratorState generatorState,
	                                  final int packageStore, final int symbolStore) {

		final JavaMethodBuilder methodBuilder = generatorState.getCurrentMethodBuilder();
		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final Optional<PackageStruct> pkg = input.getSymbolPackage();
		final String symbolName = input.getName();

		if (pkg.isEmpty()) {
			/*
			There is alot going on here, so let me explain:

			We are storing uninterned symbols in fields to solve 2 problems I'll list below. We are dynamically creating
			 these fields below and if necessary, setting their values when they are first created and attempted to be
			 referenced in an actual method.

			Why we are doing this:
			     It kinda sucks to have to repeatedly create new SymbolStructs for objects that are parsed and analyzed
			        to be the same instance. What I mean by this, is basically the Reader and the Semantic Analyzer that
			        are Lisp-like in their nature, know how to handle values correctly. However, Java is not so smart.
			        Thus when we create classes dynamically, we have to ensure that we create and reference the "same"
			        object, as is done during runtime.
			     Java does not support "Value" types. It MAY support this in Java 10, but we're not there yet. Symbols
			        could be updated to be a "Value" type, and that could possibly take care of this 'identity' crisis.
			     There may be a better way to do this, but I'd have to probably re-architect the entire way the compiler
			        analyzes and generates code. I'm not about to do that any time soon, for something like this at least.
			 */

			final String realSymbolName = symbolName.replace('-', '_')
			                                        .chars()
			                                        .filter(Character::isJavaIdentifierPart)
			                                        .mapToObj(e -> (char) e)
			                                        .map(String::valueOf)
			                                        .collect(Collectors.joining());

			final int symbolIdentityHashCode = System.identityHashCode(input);
			final String symbolNameWithHashCode = realSymbolName + '_' + symbolIdentityHashCode;

			final JavaClassBuilder currentClassBuilder = generatorState.getCurrentClassBuilder();

			final String className = currentClassBuilder.getClassName();
			final Map<String, Set<Integer>> nonPackageSymbolFields = currentClassBuilder.getNonPackageSymbolFields();

			Set<Integer> symbolHashCodeFields = nonPackageSymbolFields.get(symbolName);

			if (symbolHashCodeFields == null) {
				symbolHashCodeFields = new HashSet<>();
				nonPackageSymbolFields.put(symbolName, symbolHashCodeFields);
			}
			if (!symbolHashCodeFields.contains(symbolIdentityHashCode)) {
				final ClassWriter cw = currentClassBuilder.getClassWriter();

				// CREATE SYMBOL FIELD
				final FieldVisitor fv = cw.visitField(Opcodes.ACC_PRIVATE,
				                                      symbolNameWithHashCode,
				                                      GenerationConstants.SYMBOL_STRUCT_DESC,
				                                      null,
				                                      null);
				fv.visitEnd();

				// INIT SYMBOL FIELD VALUE
				mv.visitVarInsn(Opcodes.ALOAD, 0); // '0' is always 'this'
				mv.visitLdcInsn(symbolName);
				mv.visitMethodInsn(Opcodes.INVOKESTATIC,
				                   GenerationConstants.SYMBOL_STRUCT_NAME,
				                   GenerationConstants.SYMBOL_STRUCT_TO_SYMBOL_METHOD_NAME,
				                   GenerationConstants.SYMBOL_STRUCT_TO_SYMBOL_METHOD_DESC,
				                   true);
				mv.visitFieldInsn(Opcodes.PUTFIELD,
				                  className,
				                  symbolNameWithHashCode,
				                  GenerationConstants.SYMBOL_STRUCT_DESC);

				symbolHashCodeFields.add(symbolIdentityHashCode);
			}

			mv.visitVarInsn(Opcodes.ALOAD, 0);
			mv.visitFieldInsn(Opcodes.GETFIELD,
			                  className,
			                  symbolNameWithHashCode,
			                  GenerationConstants.SYMBOL_STRUCT_DESC);
			mv.visitVarInsn(Opcodes.ASTORE, symbolStore);
		} else {
			final String packageName = pkg.get().getName();

			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
			                   GenerationConstants.PACKAGE_STRUCT_NAME,
			                   GenerationConstants.PACKAGE_STRUCT_FIND_PACKAGE_METHOD_NAME,
			                   GenerationConstants.PACKAGE_STRUCT_FIND_PACKAGE_METHOD_DESC,
			                   true);
			mv.visitVarInsn(Opcodes.ASTORE, packageStore);

			mv.visitVarInsn(Opcodes.ALOAD, packageStore);
			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKEINTERFACE,
			                   GenerationConstants.PACKAGE_STRUCT_NAME,
			                   GenerationConstants.PACKAGE_STRUCT_INTERN_METHOD_NAME,
			                   GenerationConstants.PACKAGE_STRUCT_INTERN_METHOD_DESC,
			                   true);
			mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL,
			                   GenerationConstants.PACKAGE_SYMBOL_STRUCT_NAME,
			                   GenerationConstants.PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_NAME,
			                   GenerationConstants.PACKAGE_SYMBOL_STRUCT_GET_SYMBOL_METHOD_DESC,
			                   false);
			mv.visitVarInsn(Opcodes.ASTORE, symbolStore);
		}
	}

	public static String getFileNameFromClassName(final String className) {
		return className.substring(className.lastIndexOf('/') + 1);
	}

	public static void generateSingletonInstanceField(final ClassWriter cw, final String classDesc) {
		final FieldVisitor fv = cw.visitField(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC,
		                                      GenerationConstants.SINGLETON_INSTANCE,
		                                      classDesc,
		                                      null,
		                                      null);

		fv.visitEnd();
	}
}

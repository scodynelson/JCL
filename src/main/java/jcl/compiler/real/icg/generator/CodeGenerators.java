/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

import jcl.compiler.real.icg.JavaMethodBuilder;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

final class CodeGenerators {

	private CodeGenerators() {
	}

	static <TYPE> String getConstructorDescription(final Class<TYPE> clazz, final Class<?>... parameterTypes) {
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

	static <TYPE> String getMethodDescription(final Class<TYPE> clazz, final String methodName, final Class<?>... parameterTypes) {
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

	static void generateSymbol(final SymbolStruct<?> input, final JavaMethodBuilder methodBuilder,
	                           final int packageStore, final int symbolStore) {

		final MethodVisitor mv = methodBuilder.getMethodVisitor();

		final PackageStruct pkg = input.getSymbolPackage();
		final String symbolName = input.getName();

		if (pkg == null) {
			mv.visitTypeInsn(Opcodes.NEW, GenerationConstants.SYMBOL_STRUCT_NAME);
			mv.visitInsn(Opcodes.DUP);
			mv.visitLdcInsn(symbolName);
			mv.visitMethodInsn(Opcodes.INVOKESPECIAL,
					GenerationConstants.SYMBOL_STRUCT_NAME,
					GenerationConstants.INIT_METHOD_NAME,
					GenerationConstants.SYMBOL_STRUCT_INIT_STRING_DESC,
					false);
			mv.visitVarInsn(Opcodes.ASTORE, symbolStore);
		} else {
			final String packageName = pkg.getName();

			mv.visitLdcInsn(packageName);
			mv.visitMethodInsn(Opcodes.INVOKESTATIC,
					GenerationConstants.PACKAGE_STRUCT_NAME,
					GenerationConstants.PACKAGE_STRUCT_FIND_PACKAGE_METHOD_NAME,
					GenerationConstants.PACKAGE_STRUCT_FIND_PACKAGE_METHOD_DESC,
					false);
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
			mv.visitVarInsn(Opcodes.ASTORE, symbolStore);
		}
	}
}

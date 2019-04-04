package jcl.lang.factory;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import jcl.lang.ArrayStruct;
import jcl.lang.BitVectorStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.LogicalPathnameStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.VectorStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.internal.BitVectorStructImpl;
import jcl.lang.internal.KeywordStructImpl;
import jcl.lang.internal.LogicalPathnameStructImpl;
import jcl.lang.internal.MultiArrayStructImpl;
import jcl.lang.internal.PackageStructImpl;
import jcl.lang.internal.PathnameStructImpl;
import jcl.lang.internal.SymbolStructImpl;
import jcl.lang.internal.VectorStructImpl;
import jcl.lang.java.JavaClassStruct;
import jcl.lang.java.JavaMethodStruct;
import jcl.lang.java.JavaNameStruct;
import jcl.lang.java.JavaObjectStruct;
import jcl.lang.pathname.PathnameDevice;
import jcl.lang.pathname.PathnameDirectory;
import jcl.lang.pathname.PathnameHost;
import jcl.lang.pathname.PathnameName;
import jcl.lang.pathname.PathnameType;
import jcl.lang.pathname.PathnameVersion;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class LispStructFactory {

	private static final Logger LOGGER = LoggerFactory.getLogger(LispStructFactory.class);

	private LispStructFactory() {
	}

	/*
	 * Array
	 */

	public static ArrayStruct toArray(final List<Integer> dimensions, final List<LispStruct> contents) {
		// TODO: Fix me
		final List<IntegerStruct> dimensionStructs = dimensions.stream()
		                                                       .map(IntegerStruct::toLispInteger)
		                                                       .collect(Collectors.toList());
		return MultiArrayStructImpl.valueOf(dimensionStructs, contents);
	}

	/*
	 * BitVector
	 */

	public static BitVectorStruct toBitVector(final String bitString) {
		return BitVectorStructImpl.valueOf(bitString);
	}

	public static BitVectorStruct toBitVector(final List<IntegerStruct> contents) {
		return BitVectorStructImpl.valueOfCont(contents);
	}

	/*
	 * JavaClassStruct
	 */

	public static JavaClassStruct toJavaClass(final String className) {
		try {
			final Class<?> javaClass = Class.forName(className);
			return JavaClassStruct.valueOf(javaClass);
		} catch (final ClassNotFoundException ex) {
			throw new ErrorException("Java Class not found for class name '" + className + "'.", ex);
		}
	}

	/*
	 * JavaMethodStruct
	 */

	public static Method toJavaReflectionMethod(final String methodName,
	                                            final Class<?> javaClass,
	                                            final Class<?>... parameterTypes) {
		final String javaClassName = javaClass.getName();
		final Method[] methods = javaClass.getMethods();

		Method matchingMethod = null;

		for (final Method method : methods) {
			final String currentMethodName = method.getName();
			if (!currentMethodName.equals(methodName)) {
				continue;
			}
			final Class<?>[] currentParamTypes = method.getParameterTypes();
			if (currentParamTypes.length != parameterTypes.length) {
				continue;
			}

			boolean matches = true;
			for (int i = 0; i < currentParamTypes.length; i++) {
				final Class<?> currentParamType = currentParamTypes[i];
				final Class<?> expectedParamType = parameterTypes[i];

				if (!currentParamType.isAssignableFrom(expectedParamType)) {
					matches = false;
				}
			}
			if (matches) {
				matchingMethod = method;
				break;
			}
		}
		if (matchingMethod == null) {
			throw new ErrorException(
					"Java Class '" + javaClassName +
							"' does not have the method '" + methodName +
							"' with parameter types '" + Arrays.toString(parameterTypes) +
							"'.");
		}
		return matchingMethod;
	}

	public static JavaMethodStruct toJavaMethod(final String methodName,
	                                            final Class<?> javaClass,
	                                            final Class<?>... parameterTypes) {
		return JavaMethodStruct.valueOf(toJavaReflectionMethod(methodName, javaClass, parameterTypes));
	}

	/*
	 * JavaNameStruct
	 */

	public static JavaNameStruct toJavaName(final String javaName) {
		return JavaNameStruct.valueOf(javaName);
	}

	/*
	 * JavaObjectStruct
	 */

	public static JavaObjectStruct toJavaObject(final Class<?> javaClass) {
		final String javaClassName = javaClass.getName();
		try {
			final Constructor<?> defaultConstructor = javaClass.getDeclaredConstructor();
			final Object newInstance = defaultConstructor.newInstance();
			return JavaObjectStruct.valueOf(newInstance);
		} catch (final NoSuchMethodException ex) {
			throw new ErrorException(
					"Java Class '" + javaClassName +
							"' does not have a default no argument constructor.", ex);
		} catch (final InvocationTargetException | InstantiationException | IllegalAccessException ex) {
			final String message = "Java Class '" + javaClassName + "' could not be instantiated.";
			LOGGER.error(message, ex);
			throw new ErrorException(message, ex);
		}
	}

	/*
	 * Keyword
	 */

	public static KeywordStruct toKeyword(final String name) {
		return KeywordStructImpl.valueOf(name);
	}

	/*
	 * Package
	 */

	public static PackageStruct toPackage(final String name) {
		return PackageStructImpl.valueOf(name);
	}

	public static PackageStruct toPackage(final String name, final List<String> nicknames) {
		return PackageStructImpl.valueOf(name, nicknames);
	}

	public static PackageStruct toPackage(final String name,
	                                      final List<String> nicknames,
	                                      final PackageStruct... useList) {
		return PackageStructImpl.valueOf(name, nicknames, useList);
	}

	public static PackageStruct toPackage(final String name,
	                                      final List<String> nicknames,
	                                      final List<PackageStruct> useList) {
		return PackageStructImpl.valueOf(name, nicknames, useList);
	}

	/*
	 * Symbol
	 */

	public static SymbolStruct toSymbol(final String name) {
		return SymbolStructImpl.valueOf(name);
	}

	/*
	 * Vector
	 */

	public static VectorStruct toVector(final List<LispStruct> contents) {
		return VectorStructImpl.valueOf(contents);
	}
}

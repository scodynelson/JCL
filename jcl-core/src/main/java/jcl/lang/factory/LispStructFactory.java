package jcl.lang.factory;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URL;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Deque;
import java.util.List;
import java.util.stream.Collectors;

import jcl.lang.ArrayStruct;
import jcl.lang.BinaryNativeStreamStruct;
import jcl.lang.BitVectorStruct;
import jcl.lang.BroadcastStreamStruct;
import jcl.lang.CharacterNativeStreamStruct;
import jcl.lang.ConcatenatedStreamStruct;
import jcl.lang.EchoStreamStruct;
import jcl.lang.EmptyStreamStruct;
import jcl.lang.FileStreamStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.JavaStreamStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.LogicalPathnameStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.StringInputStreamStruct;
import jcl.lang.StringOutputStreamStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.SynonymStreamStruct;
import jcl.lang.TwoWayStreamStruct;
import jcl.lang.URLStreamStruct;
import jcl.lang.VectorStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.internal.BitVectorStructImpl;
import jcl.lang.internal.KeywordStructImpl;
import jcl.lang.internal.LogicalPathnameStructImpl;
import jcl.lang.internal.MultiArrayStructImpl;
import jcl.lang.internal.PackageStructImpl;
import jcl.lang.internal.PathnameStructImpl;
import jcl.lang.internal.SymbolStructImpl;
import jcl.lang.internal.VariableStructImpl;
import jcl.lang.internal.VectorStructImpl;
import jcl.lang.internal.readtable.ReadtableStructImpl;
import jcl.lang.internal.stream.BinaryNativeStreamStructImpl;
import jcl.lang.internal.stream.BroadcastStreamStructImpl;
import jcl.lang.internal.stream.CharacterNativeStreamStructImpl;
import jcl.lang.internal.stream.ConcatenatedStreamStructImpl;
import jcl.lang.internal.stream.EchoStreamStructImpl;
import jcl.lang.internal.stream.EmptyStreamStructImpl;
import jcl.lang.internal.stream.FileStreamStructImpl;
import jcl.lang.internal.stream.JavaStreamStructImpl;
import jcl.lang.internal.stream.StringInputStreamStructImpl;
import jcl.lang.internal.stream.StringOutputStreamStructImpl;
import jcl.lang.internal.stream.SynonymStreamStructImpl;
import jcl.lang.internal.stream.TwoWayStreamStructImpl;
import jcl.lang.internal.stream.URLStreamStructImpl;
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
import jcl.lang.readtable.ReadtableCase;
import jcl.type.LispType;
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
	 * BinaryNativeStream
	 */

	public static BinaryNativeStreamStruct toBinaryNativeStream(final InputStream inputStream,
	                                                            final OutputStream outputStream) {
		return BinaryNativeStreamStructImpl.valueOf(inputStream, outputStream);
	}

	public static BinaryNativeStreamStruct toBinaryNativeStream(final boolean interactive,
	                                                            final InputStream inputStream,
	                                                            final OutputStream outputStream) {
		return BinaryNativeStreamStructImpl.valueOf(interactive, inputStream, outputStream);
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
	 * BroadcastStream
	 */

	public static BroadcastStreamStruct toBroadcastStream(final Deque<OutputStreamStruct> outputStreamStructs) {
		return BroadcastStreamStructImpl.valueOf(outputStreamStructs);
	}

	public static BroadcastStreamStruct toBroadcastStream(final boolean interactive,
	                                                      final Deque<OutputStreamStruct> outputStreamStructs) {
		return BroadcastStreamStructImpl.valueOf(interactive, outputStreamStructs);
	}

	/*
	 * CharacterNativeStream
	 */

	public static CharacterNativeStreamStruct toCharacterNativeStream(final InputStream inputStream,
	                                                                  final OutputStream outputStream) {
		return CharacterNativeStreamStructImpl.valueOf(inputStream, outputStream);
	}

	public static CharacterNativeStreamStruct toCharacterNativeStream(final boolean interactive,
	                                                                  final InputStream inputStream,
	                                                                  final OutputStream outputStream) {
		return CharacterNativeStreamStructImpl.valueOf(interactive, inputStream, outputStream);
	}

	/*
	 * ConcatenatedStream
	 */

	public static ConcatenatedStreamStruct toConcatenatedStream(final Deque<InputStreamStruct> inputStreamStructs) {
		return ConcatenatedStreamStructImpl.valueOf(inputStreamStructs);
	}

	public static ConcatenatedStreamStruct toConcatenatedStream(final boolean interactive,
	                                                            final Deque<InputStreamStruct> inputStreamStructs) {
		return ConcatenatedStreamStructImpl.valueOf(interactive, inputStreamStructs);
	}

	/*
	 * EchoStream
	 */

	public static EchoStreamStruct toEchoStream(final InputStreamStruct inputStreamStruct,
	                                            final OutputStreamStruct outputStreamStruct) {
		return EchoStreamStructImpl.valueOf(inputStreamStruct, outputStreamStruct);
	}

	public static EchoStreamStruct toEchoStream(final boolean interactive,
	                                            final InputStreamStruct inputStreamStruct,
	                                            final OutputStreamStruct outputStreamStruct) {
		return EchoStreamStructImpl.valueOf(interactive, inputStreamStruct, outputStreamStruct);
	}

	/*
	 * EmptyStream
	 */

	public static EmptyStreamStruct toEmptyStream() {
		return EmptyStreamStructImpl.INSTANCE;
	}

	/*
	 * FileStream
	 */

	public static FileStreamStruct toFileStream(final Path path) {
		return FileStreamStructImpl.valueOf(path);
	}

	public static FileStreamStruct toFileStream(final boolean interactive, final Path path) {
		return FileStreamStructImpl.valueOf(interactive, path);
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
	 * JavaStream
	 */

	public static JavaStreamStruct toJavaStream(final InputStream inputStream, final OutputStream outputStream) {
		return JavaStreamStructImpl.valueOf(inputStream, outputStream);
	}

	public static JavaStreamStruct toJavaStream(final boolean interactive,
	                                            final InputStream inputStream,
	                                            final OutputStream outputStream) {
		return JavaStreamStructImpl.valueOf(interactive, inputStream, outputStream);
	}

	/*
	 * Keyword
	 */

	public static KeywordStruct toKeyword(final String name) {
		return KeywordStructImpl.valueOf(name);
	}

	/*
	 * Logical Pathname
	 */

	public static LogicalPathnameStruct toLogicalPathname(final String pathname) {
		return LogicalPathnameStructImpl.valueOf(pathname);
	}

	public static LogicalPathnameStruct toLogicalPathname(final PathnameHost host,
	                                                      final PathnameDirectory directory,
	                                                      final PathnameName name,
	                                                      final PathnameType type,
	                                                      final PathnameVersion version) {
		return LogicalPathnameStructImpl.valueOf(host, directory, name, type, version);
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
	 * Pathname
	 */

	public static PathnameStruct toPathname(final Path path) {
		return PathnameStructImpl.valueOf(path);
	}

	public static PathnameStruct toPathname(final File file) {
		return PathnameStructImpl.valueOf(file);
	}

	public static PathnameStruct toPathname(final String pathname) {
		return PathnameStructImpl.valueOf(pathname);
	}

	public static PathnameStruct toPathname(final URI uri) {
		return PathnameStructImpl.valueOf(uri);
	}

	public static PathnameStruct toPathname(final PathnameHost host,
	                                        final PathnameDevice device,
	                                        final PathnameDirectory directory,
	                                        final PathnameName name,
	                                        final PathnameType type,
	                                        final PathnameVersion version) {
		return PathnameStructImpl.valueOf(host, device, directory, name, type, version);
	}

	/*
	 * Readtable
	 */

	public static ReadtableStruct toReadtable() {
		return ReadtableStructImpl.valueOf();
	}

	public static ReadtableStruct toReadtable(final ReadtableCase readtableCase) {
		return ReadtableStructImpl.valueOf(readtableCase);
	}

	/*
	 * StringInputStream
	 */

	public static StringInputStreamStruct toStringInputStream(final String inputString) {
		return StringInputStreamStructImpl.valueOf(inputString);
	}

	public static StringInputStreamStruct toStringInputStream(final boolean interactive, final String inputString) {
		return StringInputStreamStructImpl.valueOf(interactive, inputString);
	}

	public static StringInputStreamStruct toStringInputStream(final String inputString,
	                                                          final int current,
	                                                          final int end) {
		return StringInputStreamStructImpl.valueOf(inputString, current, end);
	}

	public static StringInputStreamStruct toStringInputStream(final boolean interactive,
	                                                          final String inputString,
	                                                          final int current,
	                                                          final int end) {
		return StringInputStreamStructImpl.valueOf(interactive, inputString, current, end);
	}

	/*
	 * StringOutputStream
	 */

	public static StringOutputStreamStruct toStringOutputStream() {
		return StringOutputStreamStructImpl.valueOf();
	}

	public static StringOutputStreamStruct toStringOutputStream(final boolean interactive) {
		return StringOutputStreamStructImpl.valueOf(interactive);
	}

	public static StringOutputStreamStruct toStringOutputStream(final LispType elementType) {
		return StringOutputStreamStructImpl.valueOf(elementType);
	}

	public static StringOutputStreamStruct toStringOutputStream(final boolean interactive, final LispType elementType) {
		return StringOutputStreamStructImpl.valueOf(interactive, elementType);
	}

	/*
	 * Symbol
	 */

	public static SymbolStruct toSymbol(final String name) {
		return SymbolStructImpl.valueOf(name);
	}

	/*
	 * SynonymStream
	 */

	public static SynonymStreamStruct toSynonymStream(final VariableStructImpl<?> variable) {
		return SynonymStreamStructImpl.valueOf(variable);
	}

	public static SynonymStreamStruct toSynonymStream(final SymbolStruct symbol) {
		return SynonymStreamStructImpl.valueOf(symbol);
	}

	public static SynonymStreamStruct toSynonymStream(final boolean interactive, final SymbolStruct symbol) {
		return SynonymStreamStructImpl.valueOf(interactive, symbol);
	}

	/*
	 * TwoWayStream
	 */

	public static TwoWayStreamStruct toTwoWayStream(final InputStreamStruct inputStreamStruct,
	                                                final OutputStreamStruct outputStreamStruct) {
		return TwoWayStreamStructImpl.valueOf(inputStreamStruct, outputStreamStruct);
	}

	public static TwoWayStreamStruct toTwoWayStream(final boolean interactive,
	                                                final InputStreamStruct inputStreamStruct,
	                                                final OutputStreamStruct outputStreamStruct) {
		return TwoWayStreamStructImpl.valueOf(interactive, inputStreamStruct, outputStreamStruct);
	}

	/*
	 * URLStream
	 */

	public static URLStreamStruct toURLStream(final URL url) {
		return URLStreamStructImpl.valueOf(url);
	}

	public static URLStreamStruct toURLStream(final boolean interactive, final URL url) {
		return URLStreamStructImpl.valueOf(interactive, url);
	}

	/*
	 * Vector
	 */

	public static VectorStruct toVector(final List<LispStruct> contents) {
		return VectorStructImpl.valueOf(contents);
	}
}

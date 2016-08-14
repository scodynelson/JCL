package jcl.lang.factory;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.net.URL;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Deque;
import java.util.List;

import jcl.lang.ArrayStruct;
import jcl.lang.BinaryNativeStreamStruct;
import jcl.lang.BitVectorStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.BroadcastStreamStruct;
import jcl.lang.CharacterNativeStreamStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.ComplexStruct;
import jcl.lang.ConcatenatedStreamStruct;
import jcl.lang.ConsStruct;
import jcl.lang.EchoStreamStruct;
import jcl.lang.EmptyStreamStruct;
import jcl.lang.FileStreamStruct;
import jcl.lang.FloatStruct;
import jcl.lang.HashTableStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.JavaStreamStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.LogicalPathnameStruct;
import jcl.lang.NILStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.RandomStateStruct;
import jcl.lang.RatioStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.RealStruct;
import jcl.lang.StringInputStreamStruct;
import jcl.lang.StringOutputStreamStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.SynonymStreamStruct;
import jcl.lang.TStruct;
import jcl.lang.TwoWayStreamStruct;
import jcl.lang.URLStreamStruct;
import jcl.lang.VectorStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.function.EquatorFunctionStructBase;
import jcl.lang.internal.ArrayStructImpl;
import jcl.lang.internal.BitVectorStructImpl;
import jcl.lang.internal.CharacterStructImpl;
import jcl.lang.internal.ConsStructImpl;
import jcl.lang.internal.HashTableStructImpl;
import jcl.lang.internal.KeywordStructImpl;
import jcl.lang.internal.LogicalPathnameStructImpl;
import jcl.lang.internal.PackageStructImpl;
import jcl.lang.internal.PathnameStructImpl;
import jcl.lang.internal.StringStructImpl;
import jcl.lang.internal.SymbolStructImpl;
import jcl.lang.internal.VariableStructImpl;
import jcl.lang.internal.VectorStructImpl;
import jcl.lang.internal.number.ComplexStructImpl;
import jcl.lang.internal.number.FloatStructImpl;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.lang.internal.number.RandomStateStructImpl;
import jcl.lang.internal.number.RatioStructImpl;
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
import jcl.lang.readtable.ReadtableStructImpl;
import jcl.type.LispType;
import org.apache.commons.collections4.CollectionUtils;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.Apint;
import org.apfloat.Aprational;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class LispStructFactory {

	private static final Logger LOGGER = LoggerFactory.getLogger(LispStructFactory.class);

	private LispStructFactory() {
	}

	/*
	 * Array
	 */

	public static <T extends LispStruct> ArrayStruct<T> toArray(final List<Integer> dimensions, final List<T> contents) {
		return ArrayStructImpl.valueOf(dimensions, contents);
	}

	/*
	 * BinaryNativeStream
	 */

	public static BinaryNativeStreamStruct toBinaryNativeStream(final InputStream inputStream, final OutputStream outputStream) {
		return BinaryNativeStreamStructImpl.valueOf(inputStream, outputStream);
	}

	public static BinaryNativeStreamStruct toBinaryNativeStream(final boolean interactive, final InputStream inputStream, final OutputStream outputStream) {
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
	 * Boolean
	 */

	public static BooleanStruct toBoolean(final Boolean aBoolean) {
		if (aBoolean == null) {
			return NILStruct.INSTANCE;
		}
		return aBoolean ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}

	/*
	 * BroadcastStream
	 */

	public static BroadcastStreamStruct toBroadcastStream(final Deque<OutputStreamStruct> outputStreamStructs) {
		return BroadcastStreamStructImpl.valueOf(outputStreamStructs);
	}

	public static BroadcastStreamStruct toBroadcastStream(final boolean interactive, final Deque<OutputStreamStruct> outputStreamStructs) {
		return BroadcastStreamStructImpl.valueOf(interactive, outputStreamStructs);
	}

	/*
	 * Character
	 */

	/**
	 * Returns a CharacterStruct object with the provided {@code character} value.
	 *
	 * @param character
	 * 		the character value used to derive the {@link CharacterStruct#getCodePoint()} of the resulting CharacterStruct
	 *
	 * @return a CharacterStruct object with the provided {@code character} value
	 */
	public static CharacterStruct toCharacter(final Character character) {
		return CharacterStructImpl.valueOf(character);
	}

	/**
	 * Returns a CharacterStruct object with the provided {@code codePoint} value.
	 *
	 * @param codePoint
	 * 		the {@link CharacterStruct#getCodePoint()} value of the resulting CharacterStruct
	 *
	 * @return a CharacterStruct object with the provided {@code codePoint} value
	 */
	public static CharacterStruct toCharacter(final Integer codePoint) {
		return CharacterStructImpl.valueOf(codePoint);
	}

	/*
	 * CharacterNativeStream
	 */

	public static CharacterNativeStreamStruct toCharacterNativeStream(final InputStream inputStream, final OutputStream outputStream) {
		return CharacterNativeStreamStructImpl.valueOf(inputStream, outputStream);
	}

	public static CharacterNativeStreamStruct toCharacterNativeStream(final boolean interactive, final InputStream inputStream, final OutputStream outputStream) {
		return CharacterNativeStreamStructImpl.valueOf(interactive, inputStream, outputStream);
	}

	/*
	 * Complex
	 */

	public static ComplexStruct toComplex(final Apcomplex apcomplex, final ComplexStruct.ValueType valueType) {
		return ComplexStructImpl.valueOf(apcomplex, valueType);
	}

	public static ComplexStruct toComplex(final Apfloat real, final Apfloat imaginary, final ComplexStruct.ValueType valueType) {
		return ComplexStructImpl.valueOf(real, imaginary, valueType);
	}

	public static ComplexStruct toComplex(final RealStruct real, final RealStruct imaginary) {
		return ComplexStructImpl.valueOf(real, imaginary);
	}

	/*
	 * ConcatenatedStream
	 */

	public static ConcatenatedStreamStruct toConcatenatedStream(final Deque<InputStreamStruct> inputStreamStructs) {
		return ConcatenatedStreamStructImpl.valueOf(inputStreamStructs);
	}

	public static ConcatenatedStreamStruct toConcatenatedStream(final boolean interactive, final Deque<InputStreamStruct> inputStreamStructs) {
		return ConcatenatedStreamStructImpl.valueOf(interactive, inputStreamStructs);
	}

	/*
	 * Cons
	 */

	public static ConsStruct toCons(final LispStruct car) {
		return ConsStructImpl.valueOf(car);
	}

	public static ConsStruct toCons(final LispStruct car, final LispStruct cdr) {
		return ConsStructImpl.valueOf(car, cdr);
	}

	/*
	 * EchoStream
	 */

	public static EchoStreamStruct toEchoStream(final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		return EchoStreamStructImpl.valueOf(inputStreamStruct, outputStreamStruct);
	}

	public static EchoStreamStruct toEchoStream(final boolean interactive, final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
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
	 * Float
	 */

	/**
	 * Returns a FloatStruct object with the provided {@link Float} value.
	 *
	 * @param f
	 * 		the {@link Float} value of the resulting FloatStruct
	 *
	 * @return a FloatStruct object with the provided {@link Float} value
	 */
	public static FloatStruct toFloat(final Float f) {
		return FloatStructImpl.valueOf(f);
	}

	/**
	 * Returns a FloatStruct object with the provided {@link Double} value.
	 *
	 * @param d
	 * 		the {@link Double} value of the resulting FloatStruct
	 *
	 * @return a FloatStruct object with the provided {@link Double} value
	 */
	public static FloatStruct toFloat(final Double d) {
		return FloatStructImpl.valueOf(d);
	}

	/**
	 * Returns a FloatStruct object with the provided {@link BigDecimal} value.
	 *
	 * @param bigDecimal
	 * 		the {@link BigDecimal} value of the resulting FloatStruct
	 *
	 * @return a FloatStruct object with the provided {@link BigDecimal} value
	 */
	public static FloatStruct toFloat(final BigDecimal bigDecimal) {
		return FloatStructImpl.valueOf(bigDecimal);
	}

	/**
	 * Returns a new FloatStruct representing the provided {@link String}.
	 *
	 * @param s
	 * 		the {@link String} representing the new FloatStruct
	 *
	 * @return a new FloatStruct representing the provided {@link String}
	 */
	public static FloatStruct toFloat(final String s) {
		return FloatStructImpl.valueOf(s);
	}

	/**
	 * Returns a FloatStruct object with the provided {@link Apfloat} value.
	 *
	 * @param apfloat
	 * 		the {@link Apfloat} value of the resulting FloatStruct
	 *
	 * @return a FloatStruct object with the provided {@link Apfloat} value
	 */
	public static FloatStruct toFloat(final Apfloat apfloat) {
		return FloatStructImpl.valueOf(apfloat);
	}

	/**
	 * Returns a FloatStruct object with the provided {@link Apfloat} value.
	 *
	 * @param apfloat
	 * 		the {@link Apfloat} value of the resulting FloatStruct
	 * @param prototype
	 * 		the FloatStruct to use as a prototype for the resulting floating point precision
	 * 		precision
	 *
	 * @return a FloatStruct object with the provided {@link Apfloat} value
	 */
	public static FloatStruct toFloat(final Apfloat apfloat, final FloatStruct prototype) {
		return FloatStructImpl.valueOf(apfloat, prototype);
	}

	/*
	 * HashTable
	 */

	public static HashTableStruct toHashTable(final EquatorFunctionStructBase test, final BigInteger size, final float rehashThreshold) {
		return HashTableStructImpl.valueOf(test, size, rehashThreshold);
	}

	/*
	 * Integer
	 */

	/**
	 * Returns a new IntegerStruct representing the provided {@link Integer}.
	 *
	 * @param i
	 * 		the {@link Integer} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@link Integer}
	 */
	public static IntegerStruct toInteger(final Integer i) {
		return IntegerStructImpl.valueOf(i);
	}

	/**
	 * Returns a new IntegerStruct representing the provided {@link Long}.
	 *
	 * @param l
	 * 		the {@link Long} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@link Long}
	 */
	public static IntegerStruct toInteger(final Long l) {
		return IntegerStructImpl.valueOf(l);
	}

	/**
	 * Returns a new IntegerStruct representing the provided {@link BigInteger}.
	 *
	 * @param bigInteger
	 * 		the {@link BigInteger} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@link BigInteger}
	 */
	public static IntegerStruct toInteger(final BigInteger bigInteger) {
		return IntegerStructImpl.valueOf(bigInteger);
	}

	/**
	 * Returns a new IntegerStruct representing the provided {@link String}.
	 *
	 * @param s
	 * 		the {@link String} representing the new IntegerStruct
	 *
	 * @return a new IntegerStruct representing the provided {@link String}
	 */
	public static IntegerStruct toInteger(final String s) {
		return IntegerStructImpl.valueOf(s);
	}

	/**
	 * Returns a IntegerStruct object with the provided {@link Apint} value.
	 *
	 * @param apint
	 * 		the {@link Apint} value of the resulting IntegerStruct
	 *
	 * @return a IntegerStruct object with the provided {@link Apint} value
	 */
	public static IntegerStruct toInteger(final Apint apint) {
		return IntegerStructImpl.valueOf(apint);
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

	public static JavaMethodStruct toJavaMethod(final String methodName, final Class<?> javaClass, final Class<?>... parameterTypes) {
		final String javaClassName = javaClass.getName();
		try {
			final Method method = javaClass.getDeclaredMethod(methodName, parameterTypes);
			return JavaMethodStruct.valueOf(method);
		} catch (final NoSuchMethodException ex) {
			throw new ErrorException("Java Class '" + javaClassName + "' does not have the method '" + methodName + "' with parameter types '" + Arrays.toString(parameterTypes) + "'.", ex);
		}
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
			throw new ErrorException("Java Class '" + javaClassName + "' does not have a default no argument constructor.", ex);
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

	public static JavaStreamStruct toJavaStream(final boolean interactive, final InputStream inputStream, final OutputStream outputStream) {
		return JavaStreamStructImpl.valueOf(interactive, inputStream, outputStream);
	}

	/*
	 * Keyword
	 */

	public static KeywordStruct toKeyword(final String name) {
		return KeywordStructImpl.valueOf(name);
	}

	/*
	 * List
	 */

	/**
	 * Builds and returns a proper list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a proper list with the provided {@code lispStructs} as the elements
	 */
	public static ListStruct toProperList(final LispStruct... lispStructs) {
		return (lispStructs.length == 0) ? NILStruct.INSTANCE : getProperList(Arrays.asList(lispStructs));
	}

	/**
	 * Builds and returns a proper list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a proper list with the provided {@code lispStructs} as the elements
	 */
	public static ListStruct toProperList(final List<? extends LispStruct> lispStructs) {
		return CollectionUtils.isEmpty(lispStructs) ? NILStruct.INSTANCE : getProperList(lispStructs);
	}

	/**
	 * Builds and returns a proper list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a proper list with the provided {@code lispStructs} as the elements
	 */
	private static ListStruct getProperList(final List<? extends LispStruct> lispStructs) {
		final LispStruct car = lispStructs.get(0);
		final List<? extends LispStruct> rest = lispStructs.subList(1, lispStructs.size());

		final LispStruct cdr = CollectionUtils.isEmpty(rest) ? NILStruct.INSTANCE : getProperList(rest);
		return ConsStructImpl.valueOf(car, cdr);
	}

	/**
	 * Builds and returns a dotted list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a dotted list with the provided {@code lispStructs} as the elements
	 */
	public static ListStruct toDottedList(final LispStruct... lispStructs) {
		if (lispStructs.length == 0) {
			return NILStruct.INSTANCE;
		} else if (lispStructs.length == 1) {
			final LispStruct firstElement = lispStructs[0];
			if (firstElement instanceof ListStruct) {
				return (ListStruct) firstElement;
			}
			return ConsStructImpl.valueOf(firstElement, NILStruct.INSTANCE);
		} else {
			return getDottedList(Arrays.asList(lispStructs));
		}
	}

	/**
	 * Builds and returns a dotted list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a dotted list with the provided {@code lispStructs} as the elements
	 */
	public static ListStruct toDottedList(final List<? extends LispStruct> lispStructs) {
		if (CollectionUtils.isEmpty(lispStructs)) {
			return NILStruct.INSTANCE;
		} else if (lispStructs.size() == 1) {
			final LispStruct firstElement = lispStructs.get(0);
			if (firstElement instanceof ListStruct) {
				return (ListStruct) firstElement;
			}
			return ConsStructImpl.valueOf(firstElement, NILStruct.INSTANCE);
		} else {
			return getDottedList(lispStructs);
		}
	}

	/**
	 * Builds and returns a dotted list with the provided {@code lispStructs} as the elements.
	 *
	 * @param lispStructs
	 * 		the list elements
	 *
	 * @return a dotted list with the provided {@code lispStructs} as the elements
	 */
	private static ListStruct getDottedList(final List<? extends LispStruct> lispStructs) {
		final LispStruct car = lispStructs.get(0);
		final List<? extends LispStruct> rest = lispStructs.subList(1, lispStructs.size());

		final LispStruct cdr = (rest.size() == 1) ? lispStructs.get(1) : getDottedList(rest);
		return ConsStructImpl.valueOf(car, cdr);
	}

	/*
	 * Logical Pathname
	 */

	public static LogicalPathnameStruct toLogicalPathname(final String pathname) {
		return LogicalPathnameStructImpl.valueOf(pathname);
	}

	public static LogicalPathnameStruct toLogicalPathname(final PathnameHost host, final PathnameDirectory directory, final PathnameName name,
	                                                      final PathnameType type, final PathnameVersion version) {
		return LogicalPathnameStructImpl.valueOf(host, directory, name, type, version);
	}

	/*
	 * Nil
	 */

	public static NILStruct toNil() {
		return NILStruct.INSTANCE;
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

	public static PackageStruct toPackage(final String name, final List<String> nicknames, final PackageStruct... useList) {
		return PackageStructImpl.valueOf(name, nicknames, useList);
	}

	public static PackageStruct toPackage(final String name, final List<String> nicknames, final List<PackageStruct> useList) {
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

	public static PathnameStruct toPathname(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
	                                        final PathnameName name, final PathnameType type, final PathnameVersion version) {
		return PathnameStructImpl.valueOf(host, device, directory, name, type, version);
	}

	/*
	 * RandomState
	 */

	public static RandomStateStruct toRandomState() {
		return RandomStateStructImpl.valueOf();
	}

	/*
	 * Ratio
	 */

	/**
	 * Returns a new RatioStruct representing the provided {@link String}.
	 *
	 * @param s
	 * 		the {@link String} representing the new RatioStruct
	 *
	 * @return a new RatioStruct representing the provided {@link String}
	 */
	public static RatioStruct toRatio(final String s) {
		return RatioStructImpl.valueOf(s);
	}

	/**
	 * Returns a RatioStruct object with the provided {@link Aprational} value.
	 *
	 * @param aprational
	 * 		the {@link Aprational} value of the resulting RatioStruct
	 *
	 * @return a RatioStruct object with the provided {@link Aprational} value
	 */
	public static RatioStruct toRatio(final Aprational aprational) {
		return RatioStructImpl.valueOf(aprational);
	}

	/**
	 * Returns a RatioStruct object with the provided numerator and denominator {@link Apint} values.
	 *
	 * @param numerator
	 * 		the {@link Apint} value of the numerator of the resulting RatioStruct
	 * @param denominator
	 * 		the {@link Apint} value of the denominator of the resulting RatioStruct
	 *
	 * @return a RatioStruct object with the provided numerator and denominator {@link Apint} values
	 */
	public static RatioStruct toRatio(final Apint numerator, final Apint denominator) {
		return RatioStructImpl.valueOf(numerator, denominator);
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
	 * String
	 */

	public static StringStruct toString(final String stringValue) {
		return StringStructImpl.valueOf(stringValue);
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

	public static StringInputStreamStruct toStringInputStream(final String inputString, final int current, final int end) {
		return StringInputStreamStructImpl.valueOf(inputString, current, end);
	}

	public static StringInputStreamStruct toStringInputStream(final boolean interactive, final String inputString, final int current, final int end) {
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

	public static TwoWayStreamStruct toTwoWayStream(final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		return TwoWayStreamStructImpl.valueOf(inputStreamStruct, outputStreamStruct);
	}

	public static TwoWayStreamStruct toTwoWayStream(final boolean interactive, final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
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

	public static <T extends LispStruct> VectorStruct<T> toVector(final List<T> contents) {
		return VectorStructImpl.valueOf(contents);
	}
}

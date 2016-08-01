package jcl.lang.factory;

import java.io.File;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.net.URL;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Deque;
import java.util.List;

import jcl.lang.ArrayStruct;
import jcl.lang.BitVectorStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.PackageStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.VariableStruct;
import jcl.lang.VectorStruct;
import jcl.lang.array.ArrayStructImpl;
import jcl.lang.array.BitVectorStructImpl;
import jcl.lang.array.StringStructImpl;
import jcl.lang.array.VectorStructImpl;
import jcl.lang.character.CharacterStructImpl;
import jcl.lang.function.EquatorFunctionStruct;
import jcl.lang.hashtable.HashTableStruct;
import jcl.lang.list.ConsStruct;
import jcl.lang.list.NILStruct;
import jcl.lang.number.ComplexStruct;
import jcl.lang.number.FloatStruct;
import jcl.lang.number.IntegerStruct;
import jcl.lang.number.RandomStateStruct;
import jcl.lang.number.RatioStruct;
import jcl.lang.number.RealStruct;
import jcl.lang.pathname.LogicalPathnameStruct;
import jcl.lang.pathname.PathnameDevice;
import jcl.lang.pathname.PathnameDirectory;
import jcl.lang.pathname.PathnameHost;
import jcl.lang.pathname.PathnameName;
import jcl.lang.pathname.PathnameStruct;
import jcl.lang.pathname.PathnameType;
import jcl.lang.pathname.PathnameVersion;
import jcl.lang.readtable.ReadtableCase;
import jcl.lang.readtable.ReadtableStruct;
import jcl.lang.stream.BinaryNativeStreamStructImpl;
import jcl.lang.stream.BroadcastStreamStructImpl;
import jcl.lang.stream.CharacterNativeStreamStructImpl;
import jcl.lang.stream.ConcatenatedStreamStructImpl;
import jcl.lang.stream.EchoStreamStructImpl;
import jcl.lang.stream.EmptyStreamStructImpl;
import jcl.lang.stream.FileStreamStructImpl;
import jcl.lang.stream.JavaStreamStructImpl;
import jcl.lang.stream.StringInputStreamStructImpl;
import jcl.lang.stream.StringOutputStreamStructImpl;
import jcl.lang.stream.SynonymStreamStructImpl;
import jcl.lang.stream.TwoWayStreamStructImpl;
import jcl.lang.stream.URLStreamStructImpl;
import jcl.type.LispType;
import org.apache.commons.collections4.CollectionUtils;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.Apint;
import org.apfloat.Aprational;

public final class LispStructFactory {

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

	public static BinaryNativeStreamStructImpl toBinaryNativeStream(final java.io.InputStream inputStream, final java.io.OutputStream outputStream) {
		return BinaryNativeStreamStructImpl.valueOf(inputStream, outputStream);
	}

	public static BinaryNativeStreamStructImpl toBinaryNativeStream(final boolean interactive, final java.io.InputStream inputStream, final java.io.OutputStream outputStream) {
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

	public static BroadcastStreamStructImpl toBroadcastStream(final Deque<OutputStreamStruct> outputStreamStructs) {
		return BroadcastStreamStructImpl.valueOf(outputStreamStructs);
	}

	public static BroadcastStreamStructImpl toBroadcastStream(final boolean interactive, final Deque<OutputStreamStruct> outputStreamStructs) {
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

	public static CharacterNativeStreamStructImpl toCharacterNativeStream(final java.io.InputStream inputStream, final java.io.OutputStream outputStream) {
		return CharacterNativeStreamStructImpl.valueOf(inputStream, outputStream);
	}

	public static CharacterNativeStreamStructImpl toCharacterNativeStream(final boolean interactive, final java.io.InputStream inputStream, final java.io.OutputStream outputStream) {
		return CharacterNativeStreamStructImpl.valueOf(interactive, inputStream, outputStream);
	}

	/*
	 * Complex
	 */

	public static ComplexStruct toComplex(final Apcomplex apcomplex, final ComplexStruct.ValueType valueType) {
		return ComplexStruct.valueOf(apcomplex, valueType);
	}

	public static ComplexStruct toComplex(final Apfloat real, final Apfloat imaginary, final ComplexStruct.ValueType valueType) {
		return ComplexStruct.valueOf(real, imaginary, valueType);
	}

	public static ComplexStruct toComplex(final RealStruct real, final RealStruct imaginary) {
		return ComplexStruct.valueOf(real, imaginary);
	}

	/*
	 * ConcatenatedStream
	 */

	public static ConcatenatedStreamStructImpl toConcatenatedStream(final Deque<InputStreamStruct> inputStreamStructs) {
		return ConcatenatedStreamStructImpl.valueOf(inputStreamStructs);
	}

	public static ConcatenatedStreamStructImpl toConcatenatedStream(final boolean interactive, final Deque<InputStreamStruct> inputStreamStructs) {
		return ConcatenatedStreamStructImpl.valueOf(interactive, inputStreamStructs);
	}

	/*
	 * Cons
	 */

	public static ConsStruct toCons(final LispStruct car) {
		return ConsStruct.valueOf(car);
	}

	public static ConsStruct toCons(final LispStruct car, final LispStruct cdr) {
		return ConsStruct.valueOf(car, cdr);
	}

	/*
	 * EchoStream
	 */

	public static EchoStreamStructImpl toEchoStream(final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		return EchoStreamStructImpl.valueOf(inputStreamStruct, outputStreamStruct);
	}

	public static EchoStreamStructImpl toEchoStream(final boolean interactive, final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		return EchoStreamStructImpl.valueOf(interactive, inputStreamStruct, outputStreamStruct);
	}

	/*
	 * EmptyStream
	 */

	public static EmptyStreamStructImpl toEmptyStream() {
		return EmptyStreamStructImpl.INSTANCE;
	}

	/*
	 * FileStream
	 */

	public static FileStreamStructImpl toFileStream(final Path path) {
		return FileStreamStructImpl.valueOf(path);
	}

	public static FileStreamStructImpl toFileStream(final boolean interactive, final Path path) {
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
		return FloatStruct.valueOf(f);
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
		return FloatStruct.valueOf(d);
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
		return FloatStruct.valueOf(bigDecimal);
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
		return FloatStruct.valueOf(s);
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
		return FloatStruct.valueOf(apfloat);
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
		return FloatStruct.valueOf(apfloat, prototype);
	}

	/*
	 * HashTable
	 */

	public static HashTableStruct toHashTable(final EquatorFunctionStruct test, final BigInteger size, final float rehashThreshold) {
		return HashTableStruct.valueOf(test, size, rehashThreshold);
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
		return IntegerStruct.valueOf(i);
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
		return IntegerStruct.valueOf(l);
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
		return IntegerStruct.valueOf(bigInteger);
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
		return IntegerStruct.valueOf(s);
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
		return IntegerStruct.valueOf(apint);
	}

	/*
	 * JavaStream
	 */

	public static JavaStreamStructImpl toJavaStream(final java.io.InputStream inputStream, final java.io.OutputStream outputStream) {
		return JavaStreamStructImpl.valueOf(inputStream, outputStream);
	}

	public static JavaStreamStructImpl toJavaStream(final boolean interactive, final java.io.InputStream inputStream, final java.io.OutputStream outputStream) {
		return JavaStreamStructImpl.valueOf(interactive, inputStream, outputStream);
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
		return ConsStruct.valueOf(car, cdr);
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
			return ConsStruct.valueOf(firstElement, NILStruct.INSTANCE);
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
			return ConsStruct.valueOf(firstElement, NILStruct.INSTANCE);
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
		return ConsStruct.valueOf(car, cdr);
	}

	/*
	 * Logical Pathname
	 */

	public static LogicalPathnameStruct toLogicalPathname(final String pathname) {
		return LogicalPathnameStruct.valueOf(pathname);
	}

	public static LogicalPathnameStruct toLogicalPathname(final PathnameHost host, final PathnameDirectory directory, final PathnameName name,
	                                                      final PathnameType type, final PathnameVersion version) {
		return LogicalPathnameStruct.valueOf(host, directory, name, type, version);
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
		return PackageStruct.valueOf(name);
	}

	public static PackageStruct toPackage(final String name, final List<String> nicknames) {
		return PackageStruct.valueOf(name, nicknames);
	}

	public static PackageStruct toPackage(final String name, final List<String> nicknames, final PackageStruct... useList) {
		return PackageStruct.valueOf(name, nicknames, useList);
	}

	public static PackageStruct toPackage(final String name, final List<String> nicknames, final List<PackageStruct> useList) {
		return PackageStruct.valueOf(name, nicknames, useList);
	}

	/*
	 * Pathname
	 */

	public static PathnameStruct toPathname(final Path path) {
		return PathnameStruct.valueOf(path);
	}

	public static PathnameStruct toPathname(final File file) {
		return PathnameStruct.valueOf(file);
	}

	public static PathnameStruct toPathname(final String pathname) {
		return PathnameStruct.valueOf(pathname);
	}

	public static PathnameStruct toPathname(final URI uri) {
		return PathnameStruct.valueOf(uri);
	}

	public static PathnameStruct toPathname(final PathnameHost host, final PathnameDevice device, final PathnameDirectory directory,
	                                        final PathnameName name, final PathnameType type, final PathnameVersion version) {
		return PathnameStruct.valueOf(host, device, directory, name, type, version);
	}

	/*
	 * RandomState
	 */

	public static RandomStateStruct toRandomState() {
		return RandomStateStruct.valueOf();
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
		return RatioStruct.valueOf(s);
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
		return RatioStruct.valueOf(aprational);
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
		return RatioStruct.valueOf(numerator, denominator);
	}

	/*
	 * Readtable
	 */

	public static ReadtableStruct toReadtable() {
		return ReadtableStruct.valueOf();
	}

	public static ReadtableStruct toReadtable(final ReadtableCase readtableCase) {
		return ReadtableStruct.valueOf(readtableCase);
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

	public static StringInputStreamStructImpl toStringInputStream(final String inputString) {
		return StringInputStreamStructImpl.valueOf(inputString);
	}

	public static StringInputStreamStructImpl toStringInputStream(final boolean interactive, final String inputString) {
		return StringInputStreamStructImpl.valueOf(interactive, inputString);
	}

	public static StringInputStreamStructImpl toStringInputStream(final String inputString, final int current, final int end) {
		return StringInputStreamStructImpl.valueOf(inputString, current, end);
	}

	public static StringInputStreamStructImpl toStringInputStream(final boolean interactive, final String inputString, final int current, final int end) {
		return StringInputStreamStructImpl.valueOf(interactive, inputString, current, end);
	}

	/*
	 * StringOutputStream
	 */

	public static StringOutputStreamStructImpl toStringOutputStream() {
		return StringOutputStreamStructImpl.valueOf();
	}

	public static StringOutputStreamStructImpl toStringOutputStream(final boolean interactive) {
		return StringOutputStreamStructImpl.valueOf(interactive);
	}

	public static StringOutputStreamStructImpl toStringOutputStream(final LispType elementType) {
		return StringOutputStreamStructImpl.valueOf(elementType);
	}

	public static StringOutputStreamStructImpl toStringOutputStream(final boolean interactive, final LispType elementType) {
		return StringOutputStreamStructImpl.valueOf(interactive, elementType);
	}

	/*
	 * Symbol
	 */

	public static SymbolStruct toSymbol(final String name) {
		return SymbolStruct.valueOf(name);
	}

	/*
	 * SynonymStream
	 */

	public static SynonymStreamStructImpl toSynonymStream(final VariableStruct<?> variable) {
		return SynonymStreamStructImpl.valueOf(variable);
	}

	public static SynonymStreamStructImpl toSynonymStream(final SymbolStruct symbol) {
		return SynonymStreamStructImpl.valueOf(symbol);
	}

	public static SynonymStreamStructImpl toSynonymStream(final boolean interactive, final SymbolStruct symbol) {
		return SynonymStreamStructImpl.valueOf(interactive, symbol);
	}

	/*
	 * TwoWayStream
	 */

	public static TwoWayStreamStructImpl toTwoWayStream(final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		return TwoWayStreamStructImpl.valueOf(inputStreamStruct, outputStreamStruct);
	}

	public static TwoWayStreamStructImpl toTwoWayStream(final boolean interactive, final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		return TwoWayStreamStructImpl.valueOf(interactive, inputStreamStruct, outputStreamStruct);
	}

	/*
	 * URLStream
	 */

	public static URLStreamStructImpl toURLStream(final URL url) {
		return URLStreamStructImpl.valueOf(url);
	}

	public static URLStreamStructImpl toURLStream(final boolean interactive, final URL url) {
		return URLStreamStructImpl.valueOf(interactive, url);
	}

	/*
	 * Vector
	 */

	public static <T extends LispStruct> VectorStruct<T> toVector(final List<T> contents) {
		return VectorStructImpl.valueOf(contents);
	}
}
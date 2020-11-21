/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import java.io.EOFException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.charset.Charset;
import java.nio.file.Path;

import jcl.lang.BinaryStreamStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStreamStruct;
import jcl.lang.FileStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.NumberStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.EndOfFileException;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.stream.ReadCharResult;
import lombok.AccessLevel;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.SystemUtils;

/**
 * The {@link FileStreamStructImpl} is the object representation of a Lisp 'file-stream' type.
 */
@Log4j2
public final class FileStreamStructImpl extends StreamStructImpl implements FileStreamStruct {

	/**
	 * The {@link Path} of the file that the {@link #randomAccessFile} interacts with.
	 */
	private final PathnameStruct pathname;

	/**
	 * The external format used containing file format details for reading and writing operations.
	 */
	private final ExternalFormat externalFormat;

	/**
	 * The {@link RandomAccessFile} used for reading and writing file information.
	 */
	private final RandomAccessFile randomAccessFile;

	/**
	 * The number of bytes per positional unit of the binary stream. For character-type files, this value will be {@code
	 * 1}.
	 */
	private final int bytesPerUnit;

	/**
	 * Whether or not the end of the stream has been reached.
	 */
	private boolean reachedEndOfStream;

	/**
	 * The last character read from the file.
	 */
	private char lastChar;

	/**
	 * Whether or not the file reads or writes characters.
	 */
	private boolean isCharacterStream;

	/**
	 * Whether or not the file reads or write bytes.
	 */
	private boolean isBinaryStream;

	/**
	 * Whether or not the file can be read from.
	 */
	private BooleanStruct isInputStream = NILStruct.INSTANCE;

	/**
	 * Whether or not the file can be written to.
	 */
	private BooleanStruct isOutputStream = NILStruct.INSTANCE;

	/**
	 * Public constructor, initializing the various attributes of the file stream.
	 *
	 * @param pathname
	 * 		the {@link PathnameStruct} indicating the location of the file
	 * @param direction
	 * 		the direction (:input, :output, :io, :probe) to interact with the file
	 * @param elementType
	 * 		the type of the elements in the underlying stream
	 * @param ifExists
	 * 		symbol indicating specialized behavior if the file currently exists
	 * @param ifDoesNotExist
	 * 		symbol indicating specialized behavior if the file does not currently exist
	 * @param externalFormat
	 * 		the external-format containing file format details
	 */
	public FileStreamStructImpl(final PathnameStruct pathname,
	                            final SymbolStruct direction,
	                            final LispStruct elementType,
	                            final SymbolStruct ifExists,
	                            final SymbolStruct ifDoesNotExist,
	                            final LispStruct externalFormat) {
		super(elementType);

		if (CommonLispSymbols.CHARACTER.eq(elementType) || CommonLispSymbols.BASE_CHAR.eq(elementType)) {
			isCharacterStream = true;
			bytesPerUnit = 1;
		} else {
			isBinaryStream = true;

			// TODO: Clean up this crazy stuff.
			final int width = ((IntegerStruct) ((ListStruct) ((ListStruct) elementType).cdr()).car()).toJavaInt();
			bytesPerUnit = width / 8;
		}

		String mode = "rw";

		final DirectionType directionType = DirectionType.fromValue(direction);
		switch (directionType) {
			case INPUT:
				isInputStream = TStruct.INSTANCE;
				mode = "r";
				break;
			case OUTPUT:
				isOutputStream = TStruct.INSTANCE;
				mode = "rw";
				break;
			case IO:
				isInputStream = TStruct.INSTANCE;
				isOutputStream = TStruct.INSTANCE;
				mode = "rw";
				break;
			case PROBE:
				break;
		}

		this.pathname = pathname;
		this.externalFormat = ExternalFormat.fromValue(externalFormat);
		try {
			final String namestring = pathname.namestring();
			final File file = new File(namestring);
			randomAccessFile = new RandomAccessFile(file, mode);
		} catch (final FileNotFoundException fnfe) {
			throw new ErrorException("Failed to open provided file.", fnfe);
		}

		try {
			if (isOutputStream.toJavaPBoolean()) {
				final IfExistsType ifExistsType = IfExistsType.fromValue(ifExists);
				final long length = randomAccessFile.length();
				if (length > 0) {
					switch (ifExistsType) {
						case OVERWRITE:
							randomAccessFile.seek(0);
							break;
						case APPEND:
							randomAccessFile.seek(length);
							break;
						default:
							randomAccessFile.setLength(0);
					}
				}
			}
		} catch (final IOException ioe) {
			throw new ErrorException("Failed to open file output.", ioe);
		}
	}

	/*
	FILE-STREAM-STRUCT
	 */

	@Override
	public PathnameStruct toPathname() {
		return pathname;
	}

	@Override
	public ExternalFormat streamExternalFormat() {
		return externalFormat;
	}

	/*
	INPUT-STREAM-STRUCT
	 */

	@Override
	public ReadCharResult readChar(final boolean eofErrorP, final LispStruct eofValue) {
		if (isBinaryStream) {
			throw new StreamErrorException(BinaryStreamStruct.OPERATION_UNSUPPORTED, this);
		}

		try {
			final int readChar = randomAccessFile.readUnsignedByte();

			if (readChar == -1) {
				reachedEndOfStream = true;

				if (eofErrorP) {
					throw new EndOfFileException(this);
				} else {
					return new ReadCharResult(eofValue);
				}
			} else {
				return getReadPeekResult(readChar);
			}
		} catch (final EOFException eofe) {
			if (eofErrorP) {
				throw new EndOfFileException(eofe, this);
			} else {
				return new ReadCharResult(eofValue);
			}
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to read next character.", ioe, this);
		}
	}

	private ReadCharResult getReadPeekResult(final int readChar) {
		final ReadCharResult readCharResult = new ReadCharResult(readChar);

		if ((readChar == '\r') && SystemUtils.IS_OS_WINDOWS) {
			final ReadCharResult tempReadCharResult = readChar(false, null);
			if (tempReadCharResult.isEof()) {
				return readCharResult;
			}

			final Integer result = tempReadCharResult.getResult();
			if (result == '\n') {
				lineNumber++;
				return tempReadCharResult;
			} else {
				unreadChar(result);
				return readCharResult;
			}
		}

		if (readChar == '\n') {
			lineNumber++;
		}
		return readCharResult;
	}

	@Override
	public ReadCharResult readCharNoHang(final boolean eofErrorP, final LispStruct eofValue) {
		return readChar(eofErrorP, eofValue);
	}

	@Override
	public ReadCharResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		if (isCharacterStream) {
			throw new StreamErrorException(CharacterStreamStruct.OPERATION_UNSUPPORTED, this);
		}

		try {
			final int readByte = randomAccessFile.readByte();

			if (readByte == -1) {
				reachedEndOfStream = true;

				if (eofErrorP) {
					throw new EndOfFileException(this);
				} else {
					return new ReadCharResult(eofValue);
				}
			} else {
				return new ReadCharResult(readByte);
			}
		} catch (final EOFException eofe) {
			if (eofErrorP) {
				throw new EndOfFileException(eofe, this);
			} else {
				return new ReadCharResult(eofValue);
			}
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to read next byte.", ioe, this);
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		if (isBinaryStream) {
			throw new StreamErrorException(BinaryStreamStruct.OPERATION_UNSUPPORTED, this);
		}

		try {
			randomAccessFile.seek(randomAccessFile.getFilePointer() - 1);
			return codePoint;
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to unread previous character.", ioe, this);
		}
	}

	@Override
	public BooleanStruct listen() {
		if (reachedEndOfStream) {
			return NILStruct.INSTANCE;
		}

		try {
			if (isCharacterStream) {
				final int n = readChar(false, null).getResult();
				if (n < 0) {
					return NILStruct.INSTANCE;
				}

				unreadChar(n);

				return TStruct.INSTANCE;
			}
			if (isBinaryStream) {
				final long available = randomAccessFile.length() - randomAccessFile.getFilePointer();
				return (available == 0) ? TStruct.INSTANCE : NILStruct.INSTANCE;
			}
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to listen on stream.", ioe, this);
		}

		return NILStruct.INSTANCE;
	}

	/*
	OUTPUT-STREAM-STRUCT
	 */

	@Override
	public void writeChar(final int codePoint) {
		if (isBinaryStream) {
			throw new StreamErrorException(BinaryStreamStruct.OPERATION_UNSUPPORTED, this);
		}

		try {
			if ((codePoint == '\n') && SystemUtils.IS_OS_WINDOWS && (lastChar != '\r')) {
				randomAccessFile.writeChar('\r');
				randomAccessFile.writeChar('\n');
				lastChar = '\n';
			} else {
				randomAccessFile.writeChar((char) codePoint);
				lastChar = (char) codePoint;
			}
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to write character.", ioe, this);
		}
	}

	@Override
	public void writeByte(final int aByte) {
		if (isCharacterStream) {
			throw new StreamErrorException(CharacterStreamStruct.OPERATION_UNSUPPORTED, this);
		}

		try {
			randomAccessFile.writeByte(aByte);
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to write byte.", ioe, this);
		}
	}

	@Override
	public void writeString(final String outputString) {
		if (isBinaryStream) {
			throw new StreamErrorException(BinaryStreamStruct.OPERATION_UNSUPPORTED, this);
		}

		try {
			randomAccessFile.writeChars(outputString);
			lastChar = outputString.charAt(outputString.length() - 1);
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to write string.", ioe, this);
		}
	}

	@Override
	public void writeLine(final String outputString) {
		if (isBinaryStream) {
			throw new StreamErrorException(BinaryStreamStruct.OPERATION_UNSUPPORTED, this);
		}

		try {
			randomAccessFile.writeChars(outputString);
			writeChar('\n');
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to write line.", ioe, this);
		}
	}

	@Override
	public BooleanStruct freshLine() {
		if (isBinaryStream) {
			throw new StreamErrorException(BinaryStreamStruct.OPERATION_UNSUPPORTED, this);
		}

		if (lastChar == '\n') {
			return NILStruct.INSTANCE;
		}
		writeChar('\n');
		return TStruct.INSTANCE;
	}

	@Override
	public BooleanStruct terpri() {
		if (isBinaryStream) {
			throw new StreamErrorException(BinaryStreamStruct.OPERATION_UNSUPPORTED, this);
		}

		writeChar('\n');
		return NILStruct.INSTANCE;
	}

	/*
	STREAM-STRUCT
	 */

	@Override
	public BooleanStruct close(final BooleanStruct abort) {
		try {
			randomAccessFile.close();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not close stream.", ioe, this);
		}
		if (abort.toJavaPBoolean()) {
			clearOutput();
		} else {
			forceOutput();
		}
		return super.close(abort);
	}

	@Override
	public BooleanStruct inputStreamP() {
		return isInputStream;
	}

	@Override
	public BooleanStruct outputStreamP() {
		return isOutputStream;
	}

	@Override
	public LispStruct fileLength() {
		try {
			final long length = randomAccessFile.length();
			final long fileLength = length / bytesPerUnit;
			return IntegerStruct.toLispInteger(fileLength);
		} catch (final IOException ioe) {
			final StreamErrorException see = new StreamErrorException("Could not retrieve file length.", ioe, this);
			log.warn("Could no retrieve file length.", see);
			return NILStruct.INSTANCE;
		}
	}

	@Override
	public LispStruct filePosition() {
		try {
			final long filePointer = randomAccessFile.getFilePointer();
			final long position = filePointer / bytesPerUnit;
			return IntegerStruct.toLispInteger(position);
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not retrieve file position.", ioe, this);
		}
	}

	@Override
	public BooleanStruct filePosition(final IntegerStruct position) {
		try {
			final long positionLong = position.toJavaPLong();
			final long newPosition = positionLong * bytesPerUnit;
			randomAccessFile.seek(newPosition);
			return TStruct.INSTANCE;
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not move file position.", ioe, this);
		}
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.FILE_STREAM;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.FILE_STREAM;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.FILE_STREAM) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.FILE_STREAM) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}

	/**
	 * Internal enumeration representation of a file-stream `:direction` argument.
	 */
	@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
	private enum DirectionType {

		INPUT(CommonLispSymbols.INPUT_KEYWORD),
		OUTPUT(CommonLispSymbols.OUTPUT_KEYWORD),
		IO(CommonLispSymbols.IO_KEYWORD),
		PROBE(CommonLispSymbols.PROBE_KEYWORD);

		/**
		 * The matching keyword for `:direction`.
		 */
		private final SymbolStruct keyword;

		/**
		 * Returns the matching direction enumeration for the provided keyword.
		 *
		 * @param keyword
		 * 		the keyword symbol to match to an existing direction enumeration
		 *
		 * @return the matching direction enumeration for the provided keyword
		 */
		static DirectionType fromValue(final SymbolStruct keyword) {
			for (final DirectionType directionType : values()) {
				if (directionType.keyword.eq(keyword)) {
					return directionType;
				}
			}
			throw new TypeErrorException("Unknown :direction type.");
		}
	}

	/**
	 * Internal enumeration representation of a file-stream `:if-exists` argument.
	 */
	@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
	private enum IfExistsType {

		ERROR(CommonLispSymbols.ERROR_KEYWORD),
		NEW_VERSION(CommonLispSymbols.NEW_VERSION_KEYWORD),
		RENAME(CommonLispSymbols.RENAME_KEYWORD),
		RENAME_AND_DELETE(CommonLispSymbols.RENAME_AND_DELETE_KEYWORD),
		OVERWRITE(CommonLispSymbols.OVERWRITE_KEYWORD),
		APPEND(CommonLispSymbols.APPEND_KEYWORD),
		SUPERSEDE(CommonLispSymbols.SUPERSEDE_KEYWORD),
		NIL(NILStruct.INSTANCE);

		/**
		 * The matching keyword for `:if-exists`.
		 */
		private final SymbolStruct keyword;

		/**
		 * Returns the matching if-exists enumeration for the provided keyword.
		 *
		 * @param keyword
		 * 		the keyword symbol to match to an existing if-exists enumeration
		 *
		 * @return the matching if-exists enumeration for the provided keyword
		 */
		static IfExistsType fromValue(final SymbolStruct keyword) {
			for (final IfExistsType ifExistsType : values()) {
				if (ifExistsType.keyword.eq(keyword)) {
					return ifExistsType;
				}
			}
			throw new TypeErrorException("Unknown :if-exists type.");
		}
	}

	/**
	 * Internal enumeration representation of a file-stream `:if-does-not-exist` argument.
	 */
	@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
	private enum IfDoesNotExistType {

		ERROR(CommonLispSymbols.ERROR_KEYWORD),
		CREATE(CommonLispSymbols.CREATE_KEYWORD),
		NIL(NILStruct.INSTANCE);

		/**
		 * The matching keyword for `:if-does-not-exist`.
		 */
		private final SymbolStruct keyword;

		/**
		 * Returns the matching if-does-not-exist enumeration for the provided keyword.
		 *
		 * @param keyword
		 * 		the keyword symbol to match to an existing if-does-not-exist enumeration
		 *
		 * @return the matching if-does-not-exist enumeration for the provided keyword
		 */
		static IfDoesNotExistType fromValue(final SymbolStruct keyword) {
			for (final IfDoesNotExistType ifDoesNotExistType : values()) {
				if (ifDoesNotExistType.keyword.eq(keyword)) {
					return ifDoesNotExistType;
				}
			}
			throw new TypeErrorException("Unknown :if-does-not-exist type.");
		}
	}

	/**
	 * Internal representation of a stream external-format.
	 */
	@RequiredArgsConstructor(access = AccessLevel.PRIVATE)
	private static final class ExternalFormat implements LispStruct {

		/**
		 * The singleton default external-format.
		 */
		private static final ExternalFormat DEFAULT = new ExternalFormat(Charset.defaultCharset());

		/**
		 * The {@link Charset} representing the format of read characters from the file.
		 */
		private final Charset charset;

		/**
		 * Returns a new external-format object based on the provided value.
		 *
		 * @param externalFormat
		 * 		the external-format to check and create an internal implementation of
		 *
		 * @return a new external-format object
		 */
		static ExternalFormat fromValue(final LispStruct externalFormat) {
			if (CommonLispSymbols.DEFAULT_KEYWORD.eq(externalFormat)) {
				return DEFAULT;
			}

			final String charsetName;
			if (externalFormat instanceof StringStruct) {
				charsetName = ((StringStruct) externalFormat).toJavaString();
			} else if (externalFormat instanceof SymbolStruct) {
				charsetName = ((SymbolStruct) externalFormat).getName();
			} else if (externalFormat instanceof NumberStruct) {
				charsetName = externalFormat.toString();
			} else {
				throw new TypeErrorException("Invalid external-format. Must be a valid character-set identifier.");
			}
			final Charset charset = Charset.forName(charsetName);
			return new ExternalFormat(charset);
		}

		@Override
		public String toString() {
			return charset.toString();
		}
	}
}

package jcl.system;

import jcl.LispStruct;
import jcl.compiler.real.sa.SemanticAnalyzer;
import jcl.reader.macrofunction.SharpTagReaderConstants;
import jcl.reader.Reader;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.structs.packages.PackageStruct;
import jcl.structs.streams.CharacterStreamStruct;
import jcl.structs.streams.FileStreamStruct;
import jcl.structs.streams.InputStream;
import jcl.structs.streams.ReadResult;
import jcl.structs.symbols.SpecialOperator;
import jcl.structs.symbols.variables.Variable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.io.IOException;

public final class ReadEvalPrint {

	private static final Logger LOGGER = LoggerFactory.getLogger(ReadEvalPrint.class);

	private ReadEvalPrint() {
	}

	public static Object funcall(final String... args) {
		Object temp = SpecialOperator.BLOCK;

		// get local references to the basic functions
//		Function1 eval = (Function1) CommonLispFunctions.StdFunctions.Eval;

		// The basic loop. It keeps looping until someone calls the
		// Lisp EXIT function.

		if (args.length == 1) {
			final String fileName = args[0];
			final File file = new File(fileName);

			try {
				final InputStream fileStream = new FileStreamStruct(file);
				return doStuff(fileStream, true);
			} catch (final StreamErrorException ex) {
				LOGGER.error("; WARNING: Exception condition -> {}\n", ex.getMessage(), ex);
				return null;
			}
		} else {
			try (LoggerOutputStream loggerOutputStream = new LoggerOutputStream(LOGGER)) {
				final InputStream characterStream = new CharacterStreamStruct(System.in, loggerOutputStream);
				return doStuff(characterStream, false);
			} catch (final IOException ex) {
				LOGGER.error("; WARNING: Exception condition -> {}\n", ex.getMessage(), ex);
				return null;
			}
//			catch (StreamErrorException ex) {
//				LOGGER.error("; WARNING: Exception condition -> {}", ex.getMessage(), ex);
//				return null;
//			}
		}
	}

	private static Object doStuff(final InputStream inputStream, final boolean isFile) {

		// get local references to the basic functions
//		Function1 eval = (Function1) CommonLispFunctions.StdFunctions.Eval;

		// The basic loop. It keeps looping until someone calls the
		// Lisp EXIT function.

		try {
			final Reader reader = new Reader(inputStream);

			// bind all of the interaction variables to their values
//			Variable.Star.bind(Variable.Star.getValue());
//			Variable.StarStar.bind(Variable.StarStar.getValue());
//			Variable.StarStarStar.bind(Variable.StarStarStar.getValue());
//
//			Variable.Plus.bind(Variable.Plus.getValue());
//			Variable.PlusPlus.bind(Variable.PlusPlus.getValue());
//			Variable.PlusPlusPlus.bind(Variable.PlusPlusPlus.getValue());
//
//			Variable.Slash.bind(Variable.Slash.getValue());
//			Variable.SlashSlash.bind(Variable.SlashSlash.getValue());
//			Variable.SlashSlashSlash.bind(Variable.SlashSlashSlash.getValue());
//
//			Variable.Dash.bind(null);

			// Start of the REP loop
			int lineCounter = 0;
			while (true) {
//				Object value = null;
				try {
					// THE PROMPT
					final PackageStruct pkg = Variable.PACKAGE.getValue();
					LOGGER.info("\n{}: {}> ", pkg.getName(), ++lineCounter);

					// READ --------------
					SharpTagReaderConstants.SHARP_EQUAL_TEMP_TABLE.clear();
					SharpTagReaderConstants.SHARP_EQUAL_REPL_TABLE.clear();
					SharpTagReaderConstants.SHARP_EQUAL_FINAL_TABLE.clear();

					LispStruct whatRead = null;
					try {
						if (isFile) {
							whatRead = reader.read(false, null, true);
						} else {
							whatRead = reader.read();
						}
						if (whatRead != null) {
							LOGGER.debug("READ:\n");
							LOGGER.debug("{}\n", whatRead.printStruct());
						} else {
							LOGGER.warn("; WARNING: Null response from reader");
						}
					} catch (final ReaderErrorException ex) {

						// Consume the rest of the input so we don't attempt to parse the rest of the input when an error occurs.
						ReadResult readResult = reader.readChar(false, null, true);
						Integer readChar = readResult.getResult();
						while ((readChar != null) && (readChar != -1) && (readChar != 10)) {
							readResult = reader.readChar(false, null, true);
							readChar = readResult.getResult();
						}

						LOGGER.warn("; WARNING: Reader Exception condition during Read -> {}\n", ex.getMessage(), ex);
					} catch (final Exception ex) {
						LOGGER.warn("; WARNING: Exception condition during Read -> {}\n", ex.getMessage(), ex);
						break;
					} finally {
						SharpTagReaderConstants.SHARP_EQUAL_TEMP_TABLE.clear();
						SharpTagReaderConstants.SHARP_EQUAL_REPL_TABLE.clear();
						SharpTagReaderConstants.SHARP_EQUAL_FINAL_TABLE.clear();
					}

					// bind '-' to the form just read
//					Variable.Dash.bind(whatRead);

					// EVAL --------------
//					value = eval.funcall(whatRead);

					// TEMPORARY: ANALYZER

					if (whatRead != null) {
						LispStruct whatAnalyzed = null;
						try {
							final SemanticAnalyzer sa = new SemanticAnalyzer();
							whatAnalyzed = sa.analyze(whatRead);

							if (whatAnalyzed != null) {
								LOGGER.debug("ANALYZED:\n");
//								LOGGER.debug("{}\n", whatAnalyzed.printStruct());
							} else {
								LOGGER.warn("; WARNING: Null response from analyzer");
							}
						} catch (final ReaderErrorException ex) {
							LOGGER.warn("; WARNING: Analysis Exception condition during Analyzer operation -> {}", ex.getMessage(), ex);
						} catch (final Exception ex) {
							LOGGER.warn("; WARNING: Analysis condition during Analyzer operation -> {}", ex.getMessage(), ex);
							break;
						}
					}

					// save the form evaluated and its predecessors
//					Variable.PlusPlusPlus.setValue(Variable.PlusPlus.getValue());
//					Variable.PlusPlus.setValue(Variable.Plus.getValue());
//					Variable.Plus.setValue(whatRead);

					// Now save the previous returned values - if a value was returned
//					if (value != null) {
//						Variable.StarStarStar.setValue(Variable.StarStar.getValue());
//						Variable.StarStar.setValue(Variable.Star.getValue());
//
//						Variable.SlashSlashSlash.setValue(Variable.SlashSlash.getValue());
//						Variable.SlashSlash.setValue(Variable.Slash.getValue());
//
					// There is different treatment depending on whether there were
					// multiple values returned
//						if (value instanceof Object[]) {
//							Object[] mv = (Object[]) value;
//							if (mv.length == 0) {
//								Variable.Star.setValue(null);
//							} else {
//								Variable.Star.setValue(mv[0]);
//							}
//							Variable.Slash.setValue(lisp.common.type.List.Factory.newInstance(mv));
//						} else {
//							Variable.Star.setValue(value);
//							Variable.Slash.setValue(lisp.common.type.List.Factory.newInstance(value));
//						}
//					}

					// PRINT -------------
					if (whatRead == null) {
						LOGGER.info(";-- No Value --");
//					} else if (value instanceof Object[]) {
//						final Object[] mv = (Object[]) value;
//						int count = mv.length;
//						if (count == 0) {
//							LOGGER.info(";-- No Value --");
//						} else {
//							count--;
//							for (int index = 0; index < count; index++) {
//								final Object lcl = mv[index];
//								LOGGER.info(lcl.toString());
//								LOGGER.info(" ;\n");
//							}
//							LOGGER.info(mv[count].toString());
//						}
					} else {
						final String printedValue = whatRead.printStruct();
						LOGGER.info(printedValue);
					}
				} catch (final Exception ex) {
					LOGGER.error("; WARNING: Exception condition -> {}", ex.getMessage(), ex);
//				} catch (StackOverflowError err) {
//					LOGGER.error(">> Stack Overflow Error, restarting REP function.", err);
//				} catch (OutOfMemoryError err) {
//					LOGGER.error(">> Out of Memory Error, restarting REP function.", err);
//				} catch (VerifyError ve) {
//					LOGGER.error("; ERROR: loading class, {}", ve.getMessage(), ve);
//				} finally {
//					Variable.Dash.unbind();
				}
			}
		} finally {
			// unbind all of the interaction variables to their original values
//			Variable.Star.unbind();
//			Variable.StarStar.unbind();
//			Variable.StarStarStar.unbind();
//
//			Variable.Plus.unbind();
//			Variable.PlusPlus.unbind();
//			Variable.PlusPlusPlus.unbind();
//
//			Variable.Slash.unbind();
//			Variable.SlashSlash.unbind();
//			Variable.SlashSlashSlash.unbind();
		}

		return null;
	}
}

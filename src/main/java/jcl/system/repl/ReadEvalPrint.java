package jcl.system.repl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;

import jcl.LispStruct;
import jcl.compiler.real.functions.EvalFunction;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.conditions.exceptions.ConditionException;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.streams.CharacterStreamStruct;
import jcl.streams.FileStreamStruct;
import jcl.streams.InputStream;
import jcl.streams.ReadPeekResult;
import jcl.symbols.VariableStruct;
import jcl.system.InitializeVariables;
import jcl.system.LoggerOutputStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public class ReadEvalPrint {

	private static final Logger LOGGER = LoggerFactory.getLogger(ReadEvalPrint.class);

	@Autowired
	private ApplicationContext context;

	@Autowired
	private EvalFunction evalFunction;

	@Autowired
	private Printer printer;

	public void funcall(final String... args) {
		final Class<InitializeVariables> initializeVariablesClass = InitializeVariables.class;

		if (args.length == 1) {
			final String fileName = args[0];
			final Path path = new File(fileName).toPath();

			try {
				final InputStream fileStream = new FileStreamStruct(path);
				doStuff(fileStream, true);
			} catch (final StreamErrorException ex) {
				LOGGER.error("; WARNING: Exception condition -> ", ex);
			}
		} else {
			try (LoggerOutputStream loggerOutputStream = new LoggerOutputStream(LOGGER)) {
				final InputStream characterStream = new CharacterStreamStruct(System.in, loggerOutputStream);
				doStuff(characterStream, false);
			} catch (final IOException ex) {
				LOGGER.error("; WARNING: Exception condition -> ", ex);
			} catch (final StreamErrorException ex) {
				LOGGER.error("; WARNING: Exception condition -> {}", ex.getMessage(), ex);
			}
		}
	}

	private void doStuff(final InputStream inputStream, final boolean isFile) {

		try {
			REPLVariables.DASH.bindDynamicValue(NullStruct.INSTANCE);

			REPLVariables.PLUS.bindDynamicValue(NullStruct.INSTANCE);
			REPLVariables.PLUS_PLUS.bindDynamicValue(NullStruct.INSTANCE);
			REPLVariables.PLUS_PLUS_PLUS.bindDynamicValue(NullStruct.INSTANCE);

			REPLVariables.SLASH.bindDynamicValue(NullStruct.INSTANCE);
			REPLVariables.SLASH_SLASH.bindDynamicValue(NullStruct.INSTANCE);
			REPLVariables.SLASH_SLASH_SLASH.bindDynamicValue(NullStruct.INSTANCE);

			REPLVariables.STAR.bindDynamicValue(NullStruct.INSTANCE);
			REPLVariables.STAR_STAR.bindDynamicValue(NullStruct.INSTANCE);
			REPLVariables.STAR_STAR_STAR.bindDynamicValue(NullStruct.INSTANCE);

			final Reader reader = context.getBean(Reader.class, inputStream);

			int counter = 1;
			while (true) {
				try {
					// PROMPT --------------
					final PackageStruct currentPackage = PackageVariables.PACKAGE.getValue();
					final String currentPackageName = currentPackage.getName();
					LOGGER.info("{}: {}> ", currentPackageName, counter++);

					// READ --------------
					final LispStruct whatRead;
					if (isFile) {
						whatRead = reader.read(false, null, false);
					} else {
						whatRead = reader.read(true, NullStruct.INSTANCE, false);
					}

					// bind '-' to the form just read
					REPLVariables.DASH.setDynamicValue(whatRead);

					// EVAL --------------
					final LispStruct value = evalFunction.apply(whatRead);

					// bind '**' and '***' to their appropriate values
					REPLVariables.STAR_STAR_STAR.setDynamicValue(REPLVariables.STAR_STAR.getDynamicValue());
					REPLVariables.STAR_STAR.setDynamicValue(REPLVariables.STAR.getDynamicValue());

					// bind '//' and '///' to their appropriate values
					REPLVariables.SLASH_SLASH_SLASH.setValue(REPLVariables.SLASH_SLASH.getDynamicValue());
					REPLVariables.SLASH_SLASH.setDynamicValue(REPLVariables.SLASH.getDynamicValue());

					// bind '*' and '/' values to the form just evaluated
					if (value instanceof ValuesStruct) {
						final ValuesStruct values = (ValuesStruct) value;
						REPLVariables.STAR.setDynamicValue(values.getPrimaryValue());
						REPLVariables.SLASH.setDynamicValue(ListStruct.buildProperList(values.getValuesList()));
					} else {
						REPLVariables.STAR.setDynamicValue(value);
						// null check
						REPLVariables.SLASH.setDynamicValue(ListStruct.buildProperList(value));
					}

					// bind '+' to the form just evaluated and '++' and '+++' to their appropriate values
					REPLVariables.PLUS_PLUS_PLUS.setDynamicValue(REPLVariables.PLUS_PLUS.getDynamicValue());
					REPLVariables.PLUS_PLUS.setDynamicValue(REPLVariables.PLUS.getDynamicValue());
					REPLVariables.PLUS.setDynamicValue(REPLVariables.DASH.getDynamicValue());

					if (value == null) {
						LOGGER.warn("Setting * to NIL since it had no value.");
						REPLVariables.STAR.setDynamicValue(NullStruct.INSTANCE);
					}

					// PRINT -------------
					if (value == null) {
						LOGGER.info("; -- No Value --");
					} else {
						final String printedValue = printer.print(value);
						LOGGER.info(printedValue);
					}
				} catch (final ReaderErrorException ex) {

					// Consume the rest of the input so we don't attempt to parse the rest of the input when an error occurs.
					Integer readChar;
					do {
						final ReadPeekResult readResult = reader.readChar(false, null, true);
						readChar = readResult.getResult();
					} while ((readChar != null) && (readChar != -1) && (readChar != 10));

					LOGGER.warn("; WARNING: Reader Exception condition during Read -> ", ex);
				} catch (final ConditionException ex) {
					LOGGER.warn("; WARNING: Condition Exception condition during Read -> ", ex);
				} catch (final RuntimeException ex) {
					LOGGER.error("; WARNING: Runtime Exception condition -> ", ex);
				} catch (final StackOverflowError err) {
					LOGGER.error("; Stack Overflow Error, restarting REP function.", err);
				} catch (final VerifyError err) {
					LOGGER.error("; ERROR: loading class, {}", err.getMessage(), err);
				} finally {
					REPLVariables.DASH.unbindDynamicValue();
				}
			}
		} finally {
			unbindREPLVariable(REPLVariables.STAR);
			unbindREPLVariable(REPLVariables.STAR_STAR);
			unbindREPLVariable(REPLVariables.STAR_STAR_STAR);

			unbindREPLVariable(REPLVariables.SLASH);
			unbindREPLVariable(REPLVariables.SLASH_SLASH);
			unbindREPLVariable(REPLVariables.SLASH_SLASH_SLASH);

			unbindREPLVariable(REPLVariables.PLUS);
			unbindREPLVariable(REPLVariables.PLUS_PLUS);
			unbindREPLVariable(REPLVariables.PLUS_PLUS_PLUS);

			unbindREPLVariable(REPLVariables.DASH);
		}
	}

	private void unbindREPLVariable(final VariableStruct<?> replVariable) {
		if (replVariable.hasValue()) {
			replVariable.unbindDynamicValue();
		}
	}
}

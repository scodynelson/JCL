package jcl.system.repl;

import jcl.compiler.functions.EvalFunction;
import jcl.functions.readtable.ReadFunction;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.ConditionException;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.statics.PackageVariables;
import jcl.lang.statics.REPLVariables;
import jcl.lang.statics.StreamVariables;
import jcl.lang.stream.ReadPeekResult;
import jcl.printer.Printer;
import jcl.reader.Reader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.stereotype.Component;

@Component
public class ReadEvalPrint {

	private static final Logger LOGGER = LoggerFactory.getLogger(ReadEvalPrint.class);

	@Autowired
	private ReadFunction readFunction;

	@Autowired
	private EvalFunction evalFunction;

	@Autowired
	private Reader reader;

	@Autowired
	private Printer printer;

	public void funcall(final ApplicationArguments args) {
		try {
			REPLVariables.DASH.setValue(NILStruct.INSTANCE);

			REPLVariables.PLUS.setValue(NILStruct.INSTANCE);
			REPLVariables.PLUS_PLUS.setValue(NILStruct.INSTANCE);
			REPLVariables.PLUS_PLUS_PLUS.setValue(NILStruct.INSTANCE);

			REPLVariables.SLASH.setValue(NILStruct.INSTANCE);
			REPLVariables.SLASH_SLASH.setValue(NILStruct.INSTANCE);
			REPLVariables.SLASH_SLASH_SLASH.setValue(NILStruct.INSTANCE);

			REPLVariables.STAR.setValue(NILStruct.INSTANCE);
			REPLVariables.STAR_STAR.setValue(NILStruct.INSTANCE);
			REPLVariables.STAR_STAR_STAR.setValue(NILStruct.INSTANCE);

			final InputStreamStruct inputStreamStruct = StreamVariables.STANDARD_INPUT.getVariableValue();

			int counter = 1;
			while (true) {
				try {
					// PROMPT --------------
					final PackageStruct currentPackage = PackageVariables.PACKAGE.getVariableValue();
					final String currentPackageName = currentPackage.getName();
					LOGGER.info("{}: {}> ", currentPackageName, counter++);

					// READ --------------
					final LispStruct whatRead = readFunction.read(inputStreamStruct, true, NILStruct.INSTANCE, false);

					// bind '-' to the form just read
					REPLVariables.DASH.setValue(whatRead);

					// EVAL --------------
					final LispStruct value = evalFunction.apply(whatRead);

					// bind '**' and '***' to their appropriate values
					REPLVariables.STAR_STAR_STAR.setValue(REPLVariables.STAR_STAR.getValue());
					REPLVariables.STAR_STAR.setValue(REPLVariables.STAR.getValue());

					// bind '//' and '///' to their appropriate values
					REPLVariables.SLASH_SLASH_SLASH.setValue(REPLVariables.SLASH_SLASH.getValue());
					REPLVariables.SLASH_SLASH.setValue(REPLVariables.SLASH.getValue());

					// bind '*' and '/' values to the form just evaluated
					if (value instanceof ValuesStruct) {
						final ValuesStruct values = (ValuesStruct) value;
						REPLVariables.STAR.setValue(values.getPrimaryValue());
						REPLVariables.SLASH.setValue(LispStructFactory.toProperList(values.getValuesList()));
					} else {
						REPLVariables.STAR.setValue(value);
						// null check
						REPLVariables.SLASH.setValue(LispStructFactory.toProperList(value));
					}

					// bind '+' to the form just evaluated and '++' and '+++' to their appropriate values
					REPLVariables.PLUS_PLUS_PLUS.setValue(REPLVariables.PLUS_PLUS.getValue());
					REPLVariables.PLUS_PLUS.setValue(REPLVariables.PLUS.getValue());
					REPLVariables.PLUS.setValue(REPLVariables.DASH.getValue());

					if (value == null) {
						LOGGER.warn("Setting * to NIL since it had no value.");
						REPLVariables.STAR.setValue(NILStruct.INSTANCE);
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
						final ReadPeekResult readResult = reader.readChar(inputStreamStruct, false, null, true);
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
//					REPLVariables.DASH.unbindDynamicValue();
				}
			}
		} finally {
//			unbindREPLVariable(REPLVariables.STAR);
//			unbindREPLVariable(REPLVariables.STAR_STAR);
//			unbindREPLVariable(REPLVariables.STAR_STAR_STAR);
//
//			unbindREPLVariable(REPLVariables.SLASH);
//			unbindREPLVariable(REPLVariables.SLASH_SLASH);
//			unbindREPLVariable(REPLVariables.SLASH_SLASH_SLASH);
//
//			unbindREPLVariable(REPLVariables.PLUS);
//			unbindREPLVariable(REPLVariables.PLUS_PLUS);
//			unbindREPLVariable(REPLVariables.PLUS_PLUS_PLUS);
//
//			unbindREPLVariable(REPLVariables.DASH);
		}
	}

//	private void unbindREPLVariable(final VariableStructImpl<?> replVariable) {
//		if (replVariable.hasValue()) {
//			replVariable.unbindDynamicValue();
//		}
//	}
}

package jcl.system.repl;

import jcl.compiler.function.InternalEval;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.ConditionException;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.PackageVariables;
import jcl.lang.statics.REPLVariables;
import jcl.lang.stream.ReadCharResult;
import jcl.printer.Printer;
import jcl.reader.InternalRead;
import lombok.extern.log4j.Log4j2;

@Log4j2
public class ReadEvalPrint {

	public static void funcall() {
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

			final InputStreamStruct inputStreamStruct = (InputStreamStruct) CommonLispSymbols.STANDARD_INPUT.getValue();

			int counter = 1;
			while (true) {
				try {
					// PROMPT --------------
					final PackageStruct currentPackage = PackageVariables.PACKAGE.getVariableValue();
					final String currentPackageName = currentPackage.getName();
					log.info("{}: {}> ", currentPackageName, counter++);

					// READ --------------
					final LispStruct whatRead = InternalRead.read(inputStreamStruct, true, NILStruct.INSTANCE, false);

					// bind '-' to the form just read
					REPLVariables.DASH.setValue(whatRead);

					// EVAL --------------
					final LispStruct value = InternalEval.eval(whatRead);

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
						REPLVariables.SLASH.setValue(ListStruct.toLispList(values.getValuesList()));
					} else {
						REPLVariables.STAR.setValue(value);
						// null check
						REPLVariables.SLASH.setValue(ListStruct.toLispList(value));
					}

					// bind '+' to the form just evaluated and '++' and '+++' to their appropriate values
					REPLVariables.PLUS_PLUS_PLUS.setValue(REPLVariables.PLUS_PLUS.getValue());
					REPLVariables.PLUS_PLUS.setValue(REPLVariables.PLUS.getValue());
					REPLVariables.PLUS.setValue(REPLVariables.DASH.getValue());

					if (value == null) {
						log.warn("Setting * to NIL since it had no value.");
						REPLVariables.STAR.setValue(NILStruct.INSTANCE);
					}

					// PRINT -------------
					if (value == null) {
						log.info("; -- No Value --");
					} else {
						final String printedValue = Printer.print(value);
						log.info(printedValue);
					}
				} catch (final ReaderErrorException ex) {

					// Consume the rest of the input so we don't attempt to parse the rest of the input when an error occurs.
					Integer readChar;
					do {
						final ReadCharResult readResult = inputStreamStruct.readChar(false, null);
						readChar = readResult.getResult();
					} while ((readChar != null) && (readChar != -1) && (readChar != 10));

					log.warn("; WARNING: Reader Exception condition during Read -> ", ex);
				} catch (final ConditionException ex) {
					log.warn("; WARNING: Condition Exception condition during Read -> ", ex);
				} catch (final RuntimeException ex) {
					log.error("; WARNING: Runtime Exception condition -> ", ex);
				} catch (final StackOverflowError err) {
					log.error("; Stack Overflow Error, restarting REP function.", err);
				} catch (final VerifyError err) {
					log.error("; ERROR: loading class, {}", err.getMessage(), err);
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

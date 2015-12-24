package jcl.system.repl;

import jcl.LispStruct;
import jcl.compiler.functions.EvalFunction;
import jcl.compiler.struct.ValuesStruct;
import jcl.conditions.exceptions.ConditionException;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.packages.PackageStruct;
import jcl.packages.PackageVariables;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.functions.ReadFunction;
import jcl.streams.ReadPeekResult;
import jcl.streams.StreamVariables;
import jcl.streams.functions.ReadCharFunction;
import jcl.symbols.VariableStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public class ReadEvalPrint {

	private static final Logger LOGGER = LoggerFactory.getLogger(ReadEvalPrint.class);

	@Autowired
	private ApplicationContext context;

	@Autowired
	private ReadFunction readFunction;

	@Autowired
	private ReadCharFunction readCharFunction;

	@Autowired
	private EvalFunction evalFunction;

	@Autowired
	private Printer printer;

	public void funcall(final ApplicationArguments args) {
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

			final Reader reader = context.getBean(Reader.class, StreamVariables.STANDARD_INPUT.getValue());

			int counter = 1;
			while (true) {
				try {
					// PROMPT --------------
					final PackageStruct currentPackage = PackageVariables.PACKAGE.getVariableValue();
					final String currentPackageName = currentPackage.getName();
					LOGGER.info("{}: {}> ", currentPackageName, counter++);

					// READ --------------
					final LispStruct whatRead = readFunction.read(reader, true, NullStruct.INSTANCE, false);

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
						final ReadPeekResult readResult = readCharFunction.readChar(reader, false, null, true);
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

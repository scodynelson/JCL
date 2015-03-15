package jcl.printer;

import java.util.HashSet;
import java.util.Set;

import jcl.packages.GlobalPackageStruct;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.Variable;
import jcl.system.CommonLispSymbols;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

class PrintCaseVariable extends Variable<KeywordSymbolStruct> {

	public static final PrintCaseVariable INSTANCE = new PrintCaseVariable();

	private static final long serialVersionUID = 7692936629546046586L;

	private static final Set<KeywordSymbolStruct> CASE_KEYWORDS = new HashSet<>();

	static {
		CASE_KEYWORDS.add(CommonLispSymbols.UPCASE);
		CASE_KEYWORDS.add(CommonLispSymbols.DOWNCASE);
		CASE_KEYWORDS.add(CommonLispSymbols.CAPITALIZE);
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(PrintCaseVariable.class);

	private PrintCaseVariable() {
		super("*PRINT-CASE*", GlobalPackageStruct.COMMON_LISP, CommonLispSymbols.UPCASE);
	}

	@Override
	public void setValue(final KeywordSymbolStruct value) {
		if (CASE_KEYWORDS.contains(value)) {
			this.value = value;
		} else {
			LOGGER.warn("Error: *PRINT-CASE* had illegal value {}. Reset to :UPCASE", value);

			this.value = CommonLispSymbols.UPCASE;
		}
	}
}

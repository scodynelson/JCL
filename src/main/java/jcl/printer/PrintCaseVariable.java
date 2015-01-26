package jcl.printer;

import jcl.compiler.old.symbol.KeywordOld;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.KeywordSymbolStruct;
import jcl.symbols.Variable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashSet;
import java.util.Set;

class PrintCaseVariable extends Variable<KeywordSymbolStruct> {

	public static final PrintCaseVariable INSTANCE = new PrintCaseVariable();

	private static final long serialVersionUID = 7692936629546046586L;

	private static final Set<KeywordSymbolStruct> CASE_KEYWORDS = new HashSet<>();

	static {
		CASE_KEYWORDS.add(KeywordOld.Upcase);
		CASE_KEYWORDS.add(KeywordOld.Downcase);
		CASE_KEYWORDS.add(KeywordOld.Capitalize);
	}

	private static final Logger LOGGER = LoggerFactory.getLogger(PrintCaseVariable.class);

	private PrintCaseVariable() {
		super("*PRINT-CASE*", GlobalPackageStruct.COMMON_LISP, KeywordOld.Upcase);
	}

	@Override
	public void setValue(final KeywordSymbolStruct value) {
		if (CASE_KEYWORDS.contains(value)) {
			this.value = value;
		} else {
			LOGGER.warn("Error: *PRINT-CASE* had illegal value {}. Reset to :UPCASE", value);

			this.value = KeywordOld.Upcase;
		}
	}
}

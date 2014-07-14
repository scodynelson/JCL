package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ProgvAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final ProgvAnalyzer INSTANCE = new ProgvAnalyzer();

	private static final Logger LOGGER = LoggerFactory.getLogger(ProgvAnalyzer.class);

	@Override
	public LispStruct analyze(final ListStruct input) {
		LOGGER.warn("; Warning: PROGV not supported at this time.");
		return input;
	}
}

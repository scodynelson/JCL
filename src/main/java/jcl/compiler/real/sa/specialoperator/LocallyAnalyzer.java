package jcl.compiler.real.sa.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.sa.Analyzer;
import jcl.lists.ListStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LocallyAnalyzer implements Analyzer<LispStruct, ListStruct> {

	public static final LocallyAnalyzer INSTANCE = new LocallyAnalyzer();

	private static final Logger LOGGER = LoggerFactory.getLogger(LocallyAnalyzer.class);

	@Override
	public LispStruct analyze(final ListStruct input) {
		LOGGER.warn("; Warning: LOCALLY not supported at this time.");
		return input;
	}
}

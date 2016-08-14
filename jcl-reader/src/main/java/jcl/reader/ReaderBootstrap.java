package jcl.reader;

import jcl.lang.readtable.DispatchingReaderMacroFunction;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public final class ReaderBootstrap implements InitializingBean {

	private final ApplicationContext applicationContext;

	@Autowired
	public ReaderBootstrap(final ApplicationContext applicationContext) {
		this.applicationContext = applicationContext;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		final DispatchingReaderMacroFunction sharpSignDispatcher = applicationContext.getBean(DispatchingReaderMacroFunction.class);
		ReaderVariables.READTABLE.getVariableValue().makeDispatchMacroCharacter(sharpSignDispatcher, CodePointConstants.NUMBER_SIGN, false);
	}
}

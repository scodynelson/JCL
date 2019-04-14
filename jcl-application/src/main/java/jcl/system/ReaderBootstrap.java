package jcl.system;

import jcl.functions.readtable.ReadDispatchCharacterFunction;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.stereotype.Component;

@Component
public final class ReaderBootstrap implements InitializingBean {

	private final ReadDispatchCharacterFunction readDispatchCharacterFunction;

	public ReaderBootstrap(final ReadDispatchCharacterFunction readDispatchCharacterFunction) {
		this.readDispatchCharacterFunction = readDispatchCharacterFunction;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		ReaderVariables.READTABLE.getVariableValue()
		                         .makeDispatchMacroCharacter(readDispatchCharacterFunction, CodePointConstants.NUMBER_SIGN, false);
	}
}

package testground;

import jcl.lang.LispStruct;
import jcl.lang.NILStruct;

public class TestTopLevel {

	public static void main(final String[] args) {
		new TestTopLevel().init();
	}

	private void init() {
		LispStruct temp = NILStruct.INSTANCE;
	}
}

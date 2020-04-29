package jcl;

import jcl.lang.ConsStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Created by codynelson on 6/29/16.
 */
public class CoreLispStructTest {

	@Test
	public void testSomething() {
		new LispStruct() {
		};
	}

	@Test
	public void testCreateCons() {
		final ConsStruct cons = ConsStruct.toLispCons(IntegerStruct.ONE, IntegerStruct.TWO);
		Assertions.assertNotNull(cons);
	}
}
package jcl;

import jcl.lang.ConsStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import org.hamcrest.Matchers;
import org.junit.Assert;
import org.junit.Test;

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
		Assert.assertThat(cons, Matchers.is(Matchers.notNullValue()));
	}
}
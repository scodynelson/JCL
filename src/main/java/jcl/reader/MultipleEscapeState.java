package jcl.reader;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Step 6 of the Reader Algorithm.
 * <p>
 * If x is a multiple escape character then a token (initially containing no characters) is begun and step 9 is
 * entered.
 * </p>
 */
@Component
class MultipleEscapeState extends State {

	@Autowired
	private OddMultiEscapeState oddMultiEscapeState;

	@Override
	void process(final Reader reader, final TokenBuilder tokenBuilder) {
		oddMultiEscapeState.process(reader, tokenBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}

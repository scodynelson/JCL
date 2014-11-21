package jcl.reader;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Step 3 of the Reader Algorithm.
 * <p>
 * If x is a whitespace[2] character, then it is discarded and step 1 is re-entered.
 */
@Component
class WhitespaceState extends State {

	@Autowired
	private ReadState readState;

	@Override
	void process(final Reader reader, final TokenBuilder tokenBuilder) {
		readState.process(reader, tokenBuilder);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}

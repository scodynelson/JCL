import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.core.ConsoleAppender

import static ch.qos.logback.classic.Level.DEBUG

appender("A1", ConsoleAppender) {
	encoder(PatternLayoutEncoder) {
		pattern = "%m"
	}
}
root(DEBUG, ["A1"])
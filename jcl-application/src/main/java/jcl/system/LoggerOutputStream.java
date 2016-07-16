package jcl.system;

import java.io.OutputStream;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;

public class LoggerOutputStream extends OutputStream {

	private final Logger logger;

	private LogLevel logLevel;

	private String mem;

	/**
	 * Creates a new log output stream which logs bytes to the specified logger with the specified level.
	 *
	 * @param logger
	 * 		the logger where to log the written bytes
	 */
	public LoggerOutputStream(final Logger logger) {
		this(logger, LogLevel.INFO);
	}

	/**
	 * Creates a new log output stream which logs bytes to the specified logger with the specified level.
	 *
	 * @param logger
	 * 		the logger where to log the written bytes
	 * @param logLevel
	 * 		the level
	 */
	public LoggerOutputStream(final Logger logger, final LogLevel logLevel) {
		if (logger == null) {
			throw new IllegalArgumentException("Logger provided must not be null.");
		}
		this.logger = logger;
		this.logLevel = logLevel;
		mem = "";
	}

	/**
	 * Returns the logger.
	 *
	 * @return DOCUMENT ME!
	 */
	public Logger getLogger() {
		return logger;
	}

	/**
	 * Sets the logging level.
	 *
	 * @param logLevel
	 * 		the log level to use
	 */
	public void setLevel(final LogLevel logLevel) {
		this.logLevel = logLevel;
	}

	@Override
	public void write(final int b) {
		final byte[] bytes = new byte[1];
		bytes[0] = (byte) (b & 0xff);
		mem += new String(bytes);

		if (StringUtils.endsWith(mem, "\n")) {
			mem = mem.substring(0, mem.length() - 1);
			flush();
		}
	}

	@Override
	public void flush() {
		switch (logLevel) {
			case TRACE:
				logger.trace(mem);
				break;
			case DEBUG:
				logger.debug(mem);
				break;
			case INFO:
				logger.info(mem);
				break;
			case WARN:
				logger.warn(mem);
				break;
			case ERROR:
				logger.error(mem);
				break;
		}
		mem = "";
	}

	public enum LogLevel {
		TRACE,
		DEBUG,
		INFO,
		WARN,
		ERROR
	}
}

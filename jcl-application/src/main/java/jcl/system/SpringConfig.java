package jcl.system;

import jcl.printer.Printer;
import jcl.printer.PrinterImpl;
import jcl.reader.InternalRead;
import jcl.reader.Reader;
import jcl.reader.internal.ReaderImpl;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class SpringConfig {

	@Bean
	public Reader reader() {
		return new ReaderImpl();
	}

	@Bean
	public InternalRead internalRead(final Reader reader) {
		return new InternalRead(reader);
	}

	@Bean
	public Printer printer() {
		return new PrinterImpl();
	}
}

package jcl.system;

import jcl.compiler.icg.IntermediateCodeGenerator;
import jcl.compiler.icg.IntermediateCodeGeneratorImpl;
import jcl.compiler.sa.SemanticAnalyzer;
import jcl.compiler.sa.SemanticAnalyzerImpl;
import jcl.compiler.sa.analyzer.LambdaExpander;
import jcl.printer.Printer;
import jcl.printer.PrinterImpl;
import jcl.reader.InternalRead;
import jcl.reader.Reader;
import jcl.reader.internal.ReaderImpl;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

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

	@Bean
	public IntermediateCodeGenerator intermediateCodeGenerator() {
		return new IntermediateCodeGeneratorImpl();
	}

	@Bean
	@Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
	public SemanticAnalyzer semanticAnalyzer(final LambdaExpander lambdaExpander) {
		return new SemanticAnalyzerImpl(lambdaExpander);
	}
}

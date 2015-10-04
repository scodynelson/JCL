/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.integration.channel.DirectChannel;
import org.springframework.messaging.MessageChannel;

@Configuration
public class ICGConfiguration {

	@Bean
	public MessageChannel generationRequestChannel() {
		return new DirectChannel();
	}

	@Bean
	public MessageChannel generationReplyChannel() {
		return new DirectChannel();
	}

	@Bean
	public MessageChannel stringGenerationChannel() {
		return new DirectChannel();
	}

	@Bean
	public MessageChannel lambdaGenerationChannel() {
		return new DirectChannel();
	}
}

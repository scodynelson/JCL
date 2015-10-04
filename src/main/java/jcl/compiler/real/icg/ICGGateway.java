/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg;

import jcl.LispStruct;
import org.springframework.integration.annotation.Gateway;
import org.springframework.integration.annotation.MessagingGateway;
import org.springframework.messaging.handler.annotation.Header;
import org.springframework.messaging.handler.annotation.Payload;

@MessagingGateway
public interface ICGGateway {

	@Gateway(requestChannel = "generationRequestChannel", replyChannel = "generationReplyChannel")
	GeneratorState generate(@Payload LispStruct input, @Header("generatorState") GeneratorState generatorState);
}

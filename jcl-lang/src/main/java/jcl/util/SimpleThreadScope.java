package jcl.util;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.ObjectFactory;
import org.springframework.beans.factory.config.Scope;

public class SimpleThreadScope implements Scope, DisposableBean {

	private static final Log logger = LogFactory.getLog(SimpleThreadScope.class);

	private final ThreadLocal<Map<String, Object>> threadScope = ThreadLocal.withInitial(HashMap::new);

	private final Map<String, Runnable> destructionCallbacks = new LinkedHashMap<>();

	@Override
	public Object get(final String name, final ObjectFactory<?> objectFactory) {
		final Map<String, Object> scope = threadScope.get();
		Object object = scope.get(name);
		if (object == null) {
			object = objectFactory.getObject();
			scope.put(name, object);
		}
		return object;
	}

	@Override
	public Object remove(final String name) {
		final Map<String, Object> scope = threadScope.get();
		final Object object = scope.remove(name);
		if (object != null) {
			destructionCallbacks.remove(name);
		}
		return object;
	}

	@Override
	public void registerDestructionCallback(final String name, final Runnable callback) {
		destructionCallbacks.put(name, callback);
	}

	@Override
	public Object resolveContextualObject(final String key) {
		return null;
	}

	@Override
	public String getConversationId() {
		return Thread.currentThread().getName();
	}

	@Override
	public void destroy() {
		destructionCallbacks.values().forEach(Runnable::run);
		destructionCallbacks.clear();
	}
}

package org.marianoguerra.rhino;

public class TimeOutError extends Error {
	public final long timeoutMillis;

	public TimeOutError(long timeoutMillis) {
		this.timeoutMillis = timeoutMillis;
	}
}

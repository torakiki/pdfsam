/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 19/set/2013
 * Copyright 2013 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as 
 * published by the Free Software Foundation, either version 3 of the 
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.configuration;

import org.springframework.context.annotation.AnnotationConfigApplicationContext;

/**
 * IoC context holder
 * 
 * @author Andrea Vacondio
 * 
 */
public final class ApplicationContextHolder {

    private AnnotationConfigApplicationContext ctx;

    private ApplicationContextHolder() {
        ctx = new AnnotationConfigApplicationContext();
        ctx.register(LoggerConfig.class);
        ctx.register(PdfsamConfig.class);
        ctx.getEnvironment().setActiveProfiles(ctx.getEnvironment().getProperty("pdfsam.package", "BASIC"));
        ctx.register(PdfsamBasicConfig.class, PdfsamEnhancedConfig.class);
        ctx.registerShutdownHook();
        ctx.refresh();
    }

    /**
     * @return the default application context instance
     */
    public static AnnotationConfigApplicationContext getContext() {
        return DefaultApplicationContextHolder.CONTEXT.ctx;
    }

    /**
     * Lazy initialization holder class idiom (Joshua Bloch, Effective Java second edition, item 71).
     * 
     * @author Andrea Vacondio
     * 
     */
    private static final class DefaultApplicationContextHolder {

        private DefaultApplicationContextHolder() {
            // hide constructor
        }

        static final ApplicationContextHolder CONTEXT = new ApplicationContextHolder();
    }
}

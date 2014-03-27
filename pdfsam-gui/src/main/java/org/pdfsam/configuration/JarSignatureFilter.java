/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 27/mar/2014
 * Copyright 2013-2014 by Andrea Vacondio (andrea.vacondio@gmail.com).
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

import java.nio.file.Path;
import java.util.function.Predicate;

import org.pdfsam.context.DefaultI18nContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Filter that checks if the given {@link Path} is a jar and if it is signed or not. It accepts only signed jars and it can be disabled with a system property.
 * 
 * @author Andrea Vacondio
 *
 */
class JarSignatureFilter implements Predicate<Path> {
    private static final Logger LOG = LoggerFactory.getLogger(JarSignatureFilter.class);
    private static final String SKIP_SIGNATURE_CHECKING_PROP = "skip.modules.signature.check";

    public boolean test(Path p) {
        if (p == null || p.toString().toLowerCase().endsWith(".jar")) {
            return false;
        }
        if (Boolean.getBoolean(SKIP_SIGNATURE_CHECKING_PROP)) {
            LOG.info(DefaultI18nContext.getInstance().i18n("Skipping modules signature verification."));
            return true;
        }
        // TODO implement signature verification
        return false;
    }

}

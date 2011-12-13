/*
 * Created on 13/dic/2011
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam.context;

import java.util.Locale;

import org.apache.commons.lang3.StringUtils;
import org.xnap.commons.i18n.I18n;
import org.xnap.commons.i18n.I18nFactory;

/**
 * Default implementation of the {@link I18nContext}.
 * 
 * @author Andrea Vacondio
 * 
 */
public final class DefaultI18nContext implements I18nContext {

    private I18n i18n;

    private DefaultI18nContext() {
        this.i18n = I18nFactory.getI18n(DefaultI18nContext.class, getLocale());
    }

    private Locale getLocale() {
        String localeString = DefaultApplicationContext.getInstance().getLocale();
        if (StringUtils.isNotBlank(localeString)) {
            String[] i18nInfos = localeString.split("_");
            if (i18nInfos.length > 1) {
                return new Locale(i18nInfos[0].toLowerCase(), i18nInfos[1].toUpperCase());
            }
            return new Locale(i18nInfos[0].toLowerCase());
        }
        return Locale.getDefault();
    }

    /**
     * @return the default {@link I18nContext} instance
     */
    public static I18nContext getInstance() {
        return DefaultI18nContextHolder.CONTEXT;
    }

    @Override
    public I18n getI18n() {
        return i18n;
    }

    /**
     * Lazy initialization holder class idiom (Joshua Bloch, Effective Java second edition, item 71).
     * 
     * @author Andrea Vacondio
     * 
     */
    private static final class DefaultI18nContextHolder {

        private DefaultI18nContextHolder() {
            // hide constructor
        }

        static final DefaultI18nContext CONTEXT = new DefaultI18nContext();
    }
}

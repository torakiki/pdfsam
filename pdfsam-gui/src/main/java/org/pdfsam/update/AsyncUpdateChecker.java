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
package org.pdfsam.update;

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import javax.inject.Inject;
import javax.inject.Named;
import javax.swing.SwingWorker;

import org.apache.commons.lang3.StringUtils;
import org.pdfsam.context.DefaultI18nContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.core.env.Environment;

/**
 * Swing worker checking for updates asynchronously
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
@Scope(BeanDefinition.SCOPE_PROTOTYPE)
class AsyncUpdateChecker extends SwingWorker<String, Void> {
    private static final Logger LOG = LoggerFactory.getLogger(AsyncUpdateChecker.class);

    @Inject
    private UpdateChecker checker;
    @Inject
    private Environment env;

    @Override
    public String doInBackground() {
        return StringUtils.defaultString(checker.getLatestVersion());
    }

    @Override
    protected void done() {
        String latest;
        try {
            latest = get();
            if (!env.getProperty("pdfsam.version").equals(latest)) {
                eventStudio().broadcast(new UpdateAvailableEvent(latest));
            } else {
                LOG.debug(DefaultI18nContext.getInstance().i18n("No update available"));
            }
        } catch (Exception e) {
            LOG.warn("An error occurred while checking for updates");
        }

    }

}

/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 27/nov/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
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
package org.pdfsam.task;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.inject.Named;

import org.pdfsam.context.DefaultI18nContext;
import org.sejda.core.service.DefaultTaskExecutionService;
import org.sejda.core.service.TaskExecutionService;
import org.sejda.model.parameter.base.TaskParameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Component responsible for the tasks execution
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
class DefaultExecutionService implements ExecutionService {
    private static final Logger LOG = LoggerFactory.getLogger(DefaultExecutionService.class);
    private ExecutorService executor = Executors.newSingleThreadExecutor();

    @Override
    public void submit(String moduleId, TaskParameters params) {
        executor.submit(() -> {
            TaskExecutionService service = new DefaultTaskExecutionService();
            try {
                service.execute(params);
            } catch (RuntimeException re) {
                LOG.warn(DefaultI18nContext.getInstance().i18n("Unexpected error"), re);
            }
        });
    }
}

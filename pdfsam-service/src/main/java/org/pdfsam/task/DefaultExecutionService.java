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

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import javax.inject.Named;

import org.sejda.core.service.DefaultTaskExecutionService;
import org.sejda.core.service.TaskExecutionService;
import org.sejda.model.parameter.base.TaskParameters;

/**
 * Component responsible for the tasks execution
 * 
 * @author Andrea Vacondio
 * 
 */
@Named
class DefaultExecutionService implements ExecutionService {

    private ExecutorService executor = Executors.newSingleThreadExecutor();

    @Override
    public void submit(TaskParameters params) {
        executor.submit(new TaskCallable(params));

    }

    /**
     * Callable to submit for an asyc taks execution.
     * 
     * @author Andrea Vacondio
     * 
     */
    private static class TaskCallable implements Callable<Void> {
        private TaskParameters params;

        public TaskCallable(TaskParameters params) {
            this.params = params;
        }

        @Override
        public Void call() {
            TaskExecutionService service = new DefaultTaskExecutionService();
            service.execute(params);
            return null;
        }

    }
}

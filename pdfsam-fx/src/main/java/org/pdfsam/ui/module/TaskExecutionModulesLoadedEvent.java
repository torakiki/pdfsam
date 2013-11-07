/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 29/nov/2012
 * Copyright 2012 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.pdfsam.ui.module;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;


/**
 * Event sent to notify consumers that the available modules have been found and loaded by the IoC container.
 * 
 * @author Andrea Vacondio
 * 
 */
public class TaskExecutionModulesLoadedEvent {

    private List<BaseTaskExecutionModule> modules = new ArrayList<>();

    public void addAll(Collection<BaseTaskExecutionModule> modules) {
        this.modules.addAll(modules);
    }

    public List<BaseTaskExecutionModule> getModules() {
        return Collections.unmodifiableList(modules);
    }
}

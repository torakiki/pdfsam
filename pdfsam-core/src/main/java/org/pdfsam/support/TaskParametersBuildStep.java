/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/apr/2014
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
package org.pdfsam.support;

import java.util.function.Consumer;

import org.sejda.model.parameter.base.TaskParameters;

/**
 * A step in the process of building the task parameters
 * 
 * @author Andrea Vacondio
 * @param <T>
 *            type of the {@link TaskParameters} we are building
 */
public interface TaskParametersBuildStep<T extends TaskParameters> {

    /**
     * Applies changes to the input parameters and calls the provided consumer in case of error.
     * 
     * @param params
     *            parameters the builder will apply its changes to
     * @param onError
     *            function to call in case of error where the error message is supplied
     */
    void apply(T params, Consumer<String> onError);
}

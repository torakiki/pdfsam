/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 10/apr/2014
 * Copyright 2017 by Sober Lemur S.a.s. (info@pdfsam.org).
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
package org.pdfsam.support.params;

import java.util.function.Consumer;

import org.apache.commons.lang3.builder.Builder;
import org.sejda.model.parameter.base.TaskParameters;

/**
 * A step in the process of building the task parameters
 * 
 * @author Andrea Vacondio
 * @param <B>
 *            type of the builder for the {@link TaskParameters} we are building
 */
@FunctionalInterface
public interface TaskParametersBuildStep<B extends Builder<? extends TaskParameters>> {

    /**
     * Applies changes to the input parameters and calls the provided consumer in case of error.
     * 
     * @param builder
     *            parameters the builder will apply its changes to.
     * @param onError
     *            function to call in case of error where the error message is supplied
     */
    void apply(B builder, Consumer<String> onError);
}

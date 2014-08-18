/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 18/ago/2014
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
package org.pdfsam;

import org.sejda.core.service.DefaultTaskExecutionService;
import org.sejda.core.service.TaskExecutionService;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Configuration for the service module
 * 
 * @author Andrea Vacondio
 *
 */
@Configuration
public class ServiceConfig {

    @Bean
    public TaskExecutionService service() {
        return new DefaultTaskExecutionService();
    }
}

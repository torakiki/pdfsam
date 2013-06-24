/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 28/nov/2012
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
package org.pdfsam.configuration;

import org.pdfsam.module.PdfsamModule;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.ComponentScan.Filter;
import org.springframework.context.annotation.Configuration;

/**
 * IoC cofiguration
 * 
 * @author Andrea Vacondio
 * 
 */
@Configuration
@ComponentScan(basePackages = { "org.pdfsam.service", "org.pdfsam.module" }, includeFilters = @Filter(value = PdfsamModule.class))
public class AppConfig {
    // nothing
}

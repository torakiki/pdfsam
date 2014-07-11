/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 11/feb/2014
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
package org.pdfsam.ui.workarea;

import static org.pdfsam.module.ModuleDescriptorBuilder.builder;
import javafx.scene.Node;
import javafx.scene.layout.Pane;

import org.junit.Ignore;
import org.pdfsam.module.Module;
import org.pdfsam.module.ModuleCategory;
import org.pdfsam.module.ModuleDescriptor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Ignore
@Configuration
@ComponentScan(basePackages = { "org.pdfsam.ui.workarea", "org.pdfsam.module" })
public class QuickbarTestConfig {

    @Bean
    public Module testModule() {
        return new Module() {
            public Pane modulePanel() {
                return null;
            }

            public String id() {
                return "test";
            }

            public Node graphic() {
                return null;
            }

            public ModuleDescriptor descriptor() {
                return builder().category(ModuleCategory.MERGE).name("test").description("test module").build();
            }
        };
    }
}

/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 14/ago/2014
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
package org.pdfsam.update;

import static org.mockito.Matchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.timeout;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;

import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.test.InitializeJavaFxThreadRule;
import org.sejda.eventstudio.Listener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Andrea Vacondio
 *
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class DefaultUpdateServiceTest {

    @Rule
    public ClearEventStudioRule clearStudio = new ClearEventStudioRule();
    @Rule
    public InitializeJavaFxThreadRule javaFX = new InitializeJavaFxThreadRule();

    @Autowired
    private DefaultUpdateService victim;

    @Configuration
    static class Config {

        @Bean(name = "appVersion")
        public String version() {
            return "3.0.0.M1";
        }

        @Bean(name = "updatesUrl")
        public String updatesUrl() {
            return "{\"currentVersion\" : \"3.0.0\"}";
        }

        @Bean
        public DefaultUpdateService controller() {
            return new DefaultUpdateService();
        }
    }

    @Test
    public void checkForUpdates() {
        Listener<UpdateAvailableEvent> listener = mock(Listener.class);
        eventStudio().add(UpdateAvailableEvent.class, listener);
        victim.checkForUpdates();
        verify(listener, timeout(1000).times(1)).onEvent(any(UpdateAvailableEvent.class));
    }
}

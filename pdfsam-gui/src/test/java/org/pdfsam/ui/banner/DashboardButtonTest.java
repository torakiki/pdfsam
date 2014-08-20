/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 20/ago/2014
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
package org.pdfsam.ui.banner;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.Parent;

import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.runner.RunWith;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.mockito.ArgumentCaptor;
import org.pdfsam.ui.event.SetActiveDashboardItemRequest;
import org.sejda.eventstudio.Listener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import de.jensd.fx.fontawesome.AwesomeIcon;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class DashboardButtonTest extends GuiTest {

    @Autowired
    private ApplicationContext applicationContext;

    @Override
    protected Parent getRootNode() {
        return applicationContext.getBean(DashboardButton.class);
    }

    @Configuration
    static class Config {
        @Bean
        @Lazy
        public DashboardButton victim() {
            return new DashboardButton();
        }

        @Bean(name = "defaultDashboardItemId")
        public String id() {
            return "itemId";
        }
    }

    @Test
    public void testClick() {
        Listener<SetActiveDashboardItemRequest> listener = mock(Listener.class);
        eventStudio().add(SetActiveDashboardItemRequest.class, listener);
        click(AwesomeIcon.HOME.toString());
        ArgumentCaptor<SetActiveDashboardItemRequest> argument = ArgumentCaptor
                .forClass(SetActiveDashboardItemRequest.class);
        verify(listener).onEvent(argument.capture());
        assertEquals("itemId", argument.getValue().getActiveItemId());
    }
}

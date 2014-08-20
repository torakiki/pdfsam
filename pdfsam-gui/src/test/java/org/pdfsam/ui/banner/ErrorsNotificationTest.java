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

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.sejda.eventstudio.StaticStudio.eventStudio;
import javafx.scene.Parent;

import javax.inject.Inject;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.runner.RunWith;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.log.ChangedVisiblityLogAreaEvent;
import org.pdfsam.ui.log.ErrorLoggedEvent;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.Scope;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
public class ErrorsNotificationTest extends GuiTest {
    @Rule
    public ClearEventStudioRule cleanStudio = new ClearEventStudioRule();
    @Inject
    private ApplicationContext applicationContext;

    @Override
    protected Parent getRootNode() {
        return applicationContext.getBean(ErrorsNotification.class);
    }

    @Configuration
    static class Config {
        @Bean
        @Lazy
        @Scope(value = ConfigurableBeanFactory.SCOPE_PROTOTYPE)
        public ErrorsNotification victim() {
            return new ErrorsNotification();
        }
    }

    @Test
    public void onError() throws Exception {
        ErrorsNotification victim = applicationContext.getBean(ErrorsNotification.class);
        assertFalse(victim.isVisible());
        FXTestUtils.invokeAndWait(() -> eventStudio().broadcast(new ErrorLoggedEvent()), 1);
        assertTrue(victim.isVisible());
    }

    @Test
    public void onLogOpened() throws Exception {
        ErrorsNotification victim = applicationContext.getBean(ErrorsNotification.class);
        FXTestUtils.invokeAndWait(() -> eventStudio().broadcast(new ErrorLoggedEvent()), 1);
        assertTrue(victim.isVisible());
        // make sure the fade is done
        Thread.sleep(300);
        FXTestUtils.invokeAndWait(() -> eventStudio().broadcast(new ChangedVisiblityLogAreaEvent()), 1);
        Thread.sleep(300);
        assertFalse(victim.isVisible());

    }
}

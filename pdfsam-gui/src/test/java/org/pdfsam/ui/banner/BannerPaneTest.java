/* 
 * This file is part of the PDF Split And Merge source code
 * Created on 21/ago/2014
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

import static org.sejda.eventstudio.StaticStudio.eventStudio;

import java.io.IOException;

import javafx.scene.Parent;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;

import javax.inject.Inject;

import org.junit.Rule;
import org.junit.Test;
import org.junit.experimental.categories.Category;
import org.junit.runner.RunWith;
import org.loadui.testfx.GuiTest;
import org.loadui.testfx.categories.TestFX;
import org.loadui.testfx.utils.FXTestUtils;
import org.pdfsam.test.ClearEventStudioRule;
import org.pdfsam.ui.event.SetTitleEvent;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Andrea Vacondio
 *
 */
@Category(TestFX.class)
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = { MenuConfig.class, org.pdfsam.ui.banner.BannerPaneTest.Config.class })
public class BannerPaneTest extends GuiTest {
    @Rule
    public ClearEventStudioRule cleanStudio = new ClearEventStudioRule();
    @Inject
    private ApplicationContext applicationContext;

    @Override
    protected Parent getRootNode() {
        return applicationContext.getBean(BannerPane.class);
    }

    @Configuration
    @Lazy
    static class Config {
        @Bean
        public BannerPane victim() {
            return new BannerPane();
        }

        @Bean
        public ImageView payoff() throws IOException {
            return new ImageView(new ClassPathResource("/images/payoff.png").getURL().toExternalForm());
        }

        @Bean(name = "logo35")
        public Image logo35() throws IOException {
            Resource resource = new ClassPathResource("/images/logo35B.png");
            return new Image(resource.getInputStream());
        }

        @Bean
        public ErrorsNotification notification() {
            return new ErrorsNotification();
        }

        @Bean
        public LogButton logButton() {
            return new LogButton();
        }

        @Bean
        public DashboardButton dashboardButton() {
            return new DashboardButton(id());
        }

        @Bean(name = "defaultDashboardItemId")
        public String id() {
            return "itemId";
        }
    }

    @Test
    public void title() throws Exception {
        FXTestUtils.invokeAndWait(() -> eventStudio().broadcast(new SetTitleEvent("title")), 1);
        exists("@title");
    }

}

/*
 * Created on 14/dic/2011
 * Copyright 2010 by Andrea Vacondio (andrea.vacondio@gmail.com).
 *
 * This program is free software; you can redistribute it and/or modify it under the terms of the 
 * GNU General Public License as published by the Free Software Foundation; 
 * either version 2 of the License.
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
 * See the GNU General Public License for more details.
 * You should have received a copy of the GNU General Public License along with this program; 
 * if not, write to the Free Software Foundation, Inc., 
 *  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package org.pdfsam;

import java.io.File;

import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import org.apache.commons.lang3.time.DurationFormatUtils;
import org.apache.commons.lang3.time.StopWatch;
import org.bushe.swing.event.EventBus;
import org.bushe.swing.event.annotation.AnnotationProcessor;
import org.noos.xing.mydoggy.plaf.ui.util.SwingUtil;
import org.pdfsam.context.DefaultI18nContext;
import org.pdfsam.context.DefaultUserContext;
import org.pdfsam.gui.MainFrame;
import org.pdfsam.gui.WelcomePanel;
import org.pdfsam.gui.menu.MenuType;
import org.pdfsam.service.TaskExecutionRequestEvent;
import org.pdfsam.sound.PlaySoundController;
import org.sejda.model.input.PdfFileSource;
import org.sejda.model.output.DirectoryTaskOutput;
import org.sejda.model.parameter.DecryptParameters;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;

/**
 * Entry point to start pdfsam
 * 
 * @author Andrea Vacondio
 * 
 */
public final class App {
    private static final Logger LOG = LoggerFactory.getLogger(App.class);
    private static MainFrame mainFrame = null;
    private static final PlaySoundController CONTROLLER = new PlaySoundController();

    private App() {
        // hide
    }

    public static void main(String[] args) throws Exception {
        LOG.info(DefaultI18nContext.getInstance().i18n("Starting pdfsam"));
        StopWatch stopWatch = new StopWatch();
        stopWatch.start();

        try {
            initLookAndFeel();
            initSoundController();
            initService();
            mainFrame = new MainFrame();
            // TODO add plugins to Modules menu
            mainFrame.addSystemContentAction(MenuType.HELP, new WelcomePanel());

            SwingUtil.centrePositionOnScreen(mainFrame);
            mainFrame.setVisible(true);
            testSubmit();
        } catch (Exception e) {
            LOG.error(DefaultI18nContext.getInstance().i18n("Error starting pdfsam."), e);
            throw e;
        }
        stopWatch.stop();
        LOG.info(DefaultI18nContext.getInstance().i18n("Started in {0}",
                DurationFormatUtils.formatDurationWords(stopWatch.getTime(), true, true)));
    }

    private static void initLookAndFeel() throws ClassNotFoundException, InstantiationException, IllegalAccessException {
        try {
            UIManager.setLookAndFeel(DefaultUserContext.getInstance().getLookAndFeelClass());
        } catch (UnsupportedLookAndFeelException e) {
            LOG.warn(DefaultI18nContext.getInstance().i18n(
                    "Unable to install the selected look and feel because it's unsupported."));
        }

    }

    private static void initSoundController() {
        AnnotationProcessor.process(CONTROLLER);
    }

    private static void initService() {
        AnnotationConfigApplicationContext ctx = new AnnotationConfigApplicationContext();
        ctx.scan("org.pdfsam.service");
        ctx.refresh();
        ctx.registerShutdownHook();
    }

    private static void testSubmit() {
        DecryptParameters params = new DecryptParameters();
        params.setOutputPrefix("pdfsam_");
        params.setOutput(new DirectoryTaskOutput(new File("/home/torakiki/tmp")));
        params.addSource(PdfFileSource.newInstanceNoPassword(new File("/home/torakiki/atene.pdf")));
        EventBus.publish(new TaskExecutionRequestEvent(params));
    }

    /**
     * @return the mainFrame
     */
    public static MainFrame getMainFrame() {
        return mainFrame;
    }

}
